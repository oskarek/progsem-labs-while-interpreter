module Code.Inst where

type Code = [Inst]
type StmControlPoint = Int

data Inst = Inst StmControlPoint Inst' deriving (Eq, Ord)
instance Show Inst where
    show (Inst _ i) = show i
data Inst' = PUSH Integer
           | ADD
           | MULT
           | DIV
           | NEG
           | SUB
           | TRUE
           | FALSE
           | EQ'
           | LE
           | AND
           | NOT
           | FETCH String
           | STORE String
           | NOOP
           | BRANCH Code Code
           | LOOP Code Code
           | TRYCATCH Code Code
           | CATCH Code
           deriving (Eq, Ord, Show)
