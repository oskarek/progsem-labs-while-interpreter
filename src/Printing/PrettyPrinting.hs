module Printing.PrettyPrinting
    ( prettyPrint
    )
where

import           Analysis.Analysis
import           Syntax.AST
import qualified Data.Map                      as M
import           Data.List
import qualified Data.List.NonEmpty            as NE

prettyPrintA :: AExp -> String
prettyPrintA aexp = case aexp of
    Num n -> show n
    Neg a -> "-(" ++ prettyPrintA a ++ ")"
    Var x -> x
    Plus a1 a2 -> printBin " + " a1 a2
    Times a1 a2 -> printBin " * " a1 a2
    Div a1 a2 -> printBin " / " a1 a2
    Minus a1 a2 -> printBin " - " a1 a2

printBin :: String -> AExp -> AExp -> String
printBin op a1 a2 = "(" ++ prettyPrintA a1 ++ op ++ prettyPrintA a2 ++ ")"

prettyPrintB :: BExp -> String
prettyPrintB bexp = case bexp of
    TrueLit -> "true"
    FalseLit -> "false"
    Equals a1 a2 -> printBin " = " a1 a2
    LessThanEq a1 a2 -> printBin " <= " a1 a2
    Not b -> "!(" ++ prettyPrintB b ++ ")"
    And b1 b2 -> "(" ++ prettyPrintB b1 ++ " && " ++ prettyPrintB b2 ++ ")"

prettyPrint' :: (Show i, Show b) => Int -> M.Map ControlPoint (LubInfo i b) -> Stm -> String
prettyPrint' ind' infoMap (Stm cp stm) =
    let ind = (replicate (4*ind') ' ' ++)
        info = infoMap M.!? CP cp
        infoStr = maybe "(❌  unreachable code!)" show info
        unlines' = intercalate "\n"
    in case stm of
        Assign x a -> unlines' $ map ind [ infoStr, x ++ " := " ++ prettyPrintA a ]
        Skip -> unlines' $ map ind [ infoStr, "skip" ]
        Seq ss -> unlines' $ map (++";") $ prettyPrint' ind' infoMap <$> NE.toList ss
        IfThnEls cond s1 s2 -> unlines'
            [ ind infoStr
            , ind $ "if " ++ prettyPrintB cond ++ " then"
            , prettyPrint' (ind'+1) infoMap s1
            , ind "else"
            , prettyPrint' (ind'+1) infoMap s2 ]
        While cond s -> unlines'
            [ ind infoStr
            , ind $ "while " ++ prettyPrintB cond ++ " do"
            , prettyPrint' (ind'+1) infoMap s ]
        TryCatch s1 s2 -> unlines'
            [ ind infoStr
            , ind "try"
            , prettyPrint' (ind'+1) infoMap s1
            , ind "catch"
            , prettyPrint' (ind'+1) infoMap s2 ]

prettyPrint :: (Show i, Show b) => M.Map ControlPoint (LubInfo i b) -> Stm -> String
prettyPrint info stm =
    let progStr = prettyPrint' 0 info stm
        endInfo = info M.!? EndPoint
        endStr = case endInfo of
            Nothing -> " (💣 guaranteed exceptional termination!)"
            Just li@LubInfo {..} -> case cpState of
                MaybeExceptional -> show li ++ "️️️ (⚠️  potential excepional termination!)"
                Normal -> show li ++ " (✅  normal termination)"
    in progStr ++ "\n" ++ endStr