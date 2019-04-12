module Code.Inst where

type Code = [Inst]

data Inst = PUSH Integer
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
          deriving (Eq, Show)
