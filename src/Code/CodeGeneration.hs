module Code.CodeGeneration where

import           Syntax.AST
import           Code.Inst

ca :: AExp -> Code
ca a = case a of
    Num n       -> [PUSH n]
    Neg a       -> ca a ++ [NEG]
    Var x       -> [FETCH x]
    Plus  a1 a2 -> ca a2 ++ ca a1 ++ [ADD]
    Times a1 a2 -> ca a2 ++ ca a1 ++ [MULT]
    Div   a1 a2 -> ca a2 ++ ca a1 ++ [DIV]
    Minus a1 a2 -> ca a2 ++ ca a1 ++ [SUB]

cb :: BExp -> Code
cb b = case b of
    TrueLit          -> [TRUE]
    FalseLit         -> [FALSE]
    Equals     a1 a2 -> ca a2 ++ ca a1 ++ [EQ']
    LessThanEq a1 a2 -> ca a2 ++ ca a1 ++ [LE]
    Not b'           -> cb b' ++ [NOT]
    And b1 b2        -> cb b2 ++ cb b1 ++ [AND]

cs :: Stm -> Code
cs s = case s of
    Assign x a       -> ca a ++ [STORE x]
    Skip             -> [NOOP]
    Seq ss           -> foldMap cs ss
    IfThnEls b s1 s2 -> cb b ++ [BRANCH (cs s1) (cs s2)]
    While    b  s'   -> [LOOP (cb b) (cs s')]
    TryCatch s1 s2   -> [TRYCATCH (cs s1) (cs s2)]
