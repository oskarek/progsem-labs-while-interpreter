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
          | SBRANCH Code Code
          | SETOKSTATE
          deriving (Eq, Show)
