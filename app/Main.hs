{-# LANGUAGE RecordWildCards #-}
module Main where

import           Parsing.Parser                 ( whileParser )
import           Text.Megaparsec                ( parse, errorBundlePretty )
import qualified Code.CodeGeneration           as CG
import qualified AM.AbstractMachine            as AM
import           Control.Arrow                  ( (>>>) )
import           Control.Monad                  ( (>=>) )
import           Options.Applicative
import           Data.Semigroup                 ( (<>) )

data Mode = Debugging | Production
data WhileData = WD { mode :: Mode, file :: FilePath }

whileData :: Parser WhileData
whileData = WD
    <$> flag Production Debugging
          ( long "debug" <> help "Enable debugging mode" )
    <*> strOption
          ( long "path" <> metavar "PATH" <> help "Path to input file" )

interpretWhileProg :: WhileData -> IO ()
interpretWhileProg WD{..} = readFile file
    >>= (parse whileParser file
        >>> either (errorBundlePretty >>> putStr) (CG.cs >>> printExecRes))
  where
    printExecRes = case mode of
        Debugging -> AM.executionConfigs >>> mapM_ (print >=> const getChar)
        Production -> AM.execute >>> print

main :: IO ()
main = execParser opts >>= interpretWhileProg
    where opts = info (whileData <**> helper)
            ( fullDesc <> header "while-interpreter - an interpreter for While programs" )
