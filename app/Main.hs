module Main where

import           Parsing.Parser                 ( whileParser )
import           Text.Megaparsec                ( runParserT
                                                , errorBundlePretty
                                                )
import qualified Code.CodeGeneration           as CG
import qualified AM.AbstractMachine            as AM
import           Control.Arrow                  ( (>>>) )
import           Control.Monad                  ( (>=>) )
import           Options.Applicative
import           Operations.Operations          ( mIntBoolOps )
import           AM.Configuration               ( showConfig, Configuration )
import           Operations.SignExcOps          ( signExcOps )
import qualified Control.Monad.State           as State
import qualified Control.Monad.Reader          as R
import           Analysis.Analysis
import           Printing.PrettyPrinting
import           ProgramTypes.EvalWithOps

data Mode = Debugging | Analysis | Production
data WhileData = WD { mode :: Mode, abstract :: Bool, file :: FilePath }

whileData :: Parser WhileData
whileData = WD
    <$> (flag Production Debugging
          ( long "debug" <> help "Enable debugging mode" )
        <|> flag Production Analysis
        ( long "analysis" <> help "Enable analysis mode" ) )
    <*> switch ( long "abstract" <> help "Use abstract values" )
    <*> strOption
          ( long "path" <> metavar "PATH" <> help "Path to input file" )

interpretWhileProg :: WhileData -> IO ()
interpretWhileProg WD {..} = readFile file >>= (parse >>> printParseRes)
  where
    parse s = State.evalState (runParserT whileParser file s) 0
    printParseRes =
        either (errorBundlePretty >>> putStr) printExecRes
    printExecRes code = if abstract
        then R.runReaderT (unOps (printExec mode code)) signExcOps
        else R.runReaderT (unOps (printExec mode code)) mIntBoolOps

printExec mode = case mode of
  Debugging  -> debugPrint
  Analysis   -> analysisPrint
  Production -> CG.cs >>> AM.execute >=> printConfigs
 where
  debugPrint = CG.cs >>> AM.configSteps >=> mapM_
    (\c -> R.liftIO (putStrLn $ replicate 40 '-') >> printConfigs c >> R.liftIO getChar)
  analysisPrint s = R.liftIO . putStrLn . flip prettyPrint s
    =<< ctrlPointLubInfo
    =<< AM.configurationGraph (CG.cs s)

printConfigs :: (Foldable t, Show i, Show b) => t (Configuration i b) -> EvalWithOps i b ()
printConfigs t
 | null t = R.liftIO $ putStrLn "No end states"
 | otherwise = mapM_ (R.liftIO . putStrLn . showConfig) t

main :: IO ()
main = execParser opts >>= interpretWhileProg
    where opts = info (whileData <**> helper)
            ( fullDesc <> header "while-interpreter - an interpreter for While programs" )
