module Syntax.AST where

import           Data.List.NonEmpty             ( NonEmpty )

data AExp = Num Integer
          | Neg AExp
          | Var String
          | Plus AExp AExp
          | Times AExp AExp
          | Div AExp AExp
          | Minus AExp AExp
          deriving (Eq, Show)

data BExp = TrueLit
          | FalseLit
          | Equals AExp AExp
          | LessThanEq AExp AExp
          | Not BExp
          | And BExp BExp
          deriving (Eq, Show)

data Stm = Assign String AExp
         | Skip
         | Seq (NonEmpty Stm)
         | IfThnEls BExp Stm Stm
         | While BExp Stm
         | TryCatch Stm Stm
         deriving (Eq, Show)
