module Code.CodeGeneration where

import           Syntax.AST
import           Code.Inst

ca :: StmControlPoint -> AExp -> Code
ca cp a = case a of
    Num n       -> [Inst cp (PUSH n)]
    Neg a'      -> ca cp a' ++ [Inst cp NEG]
    Var x       -> [Inst cp (FETCH x)]
    Plus  a1 a2 -> ca cp a2 ++ ca cp a1 ++ [Inst cp ADD]
    Times a1 a2 -> ca cp a2 ++ ca cp a1 ++ [Inst cp MULT]
    Div   a1 a2 -> ca cp a2 ++ ca cp a1 ++ [Inst cp DIV]
    Minus a1 a2 -> ca cp a2 ++ ca cp a1 ++ [Inst cp SUB]

cb :: StmControlPoint -> BExp -> Code
cb cp b = case b of
    TrueLit          -> [Inst cp TRUE]
    FalseLit         -> [Inst cp FALSE]
    Equals     a1 a2 -> ca cp a2 ++ ca cp a1 ++ [Inst cp EQ']
    LessThanEq a1 a2 -> ca cp a2 ++ ca cp a1 ++ [Inst cp LE]
    Not b'           -> cb cp b' ++ [Inst cp NOT]
    And b1 b2        -> cb cp b2 ++ cb cp b1 ++ [Inst cp AND]

cs :: Stm -> Code
cs (Stm cp s) = case s of
    Assign x a       -> ca cp a ++ [Inst cp (STORE x)]
    Skip             -> [Inst cp NOOP]
    Seq ss           -> foldMap cs ss
    IfThnEls b s1 s2 -> cb cp b ++ [Inst cp (BRANCH (cs s1) (cs s2))]
    While    b  s'   -> [Inst cp (LOOP (cb cp b) (cs s'))]
    TryCatch s1 s2   -> [Inst cp (TRYCATCH (cs s1) (cs s2))]
