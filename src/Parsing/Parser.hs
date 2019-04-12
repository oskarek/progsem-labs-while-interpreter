module Parsing.Parser
    ( whileParser
    )
where

import           Syntax.AST
import           Data.Functor                   ( (<$) )
import           Data.Void
import           Control.Applicative            ( empty
                                                , liftA2
                                                , liftA3
                                                )
import           Control.Monad.Combinators.Expr
import           Text.Megaparsec         hiding ( sepBy1 )
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import           Control.Applicative.Combinators.NonEmpty
                                                ( sepBy1 )
import           Data.List.NonEmpty             ( NonEmpty(..) )

type Parser = Parsec Void String

-- LEXER

-- A space consumer
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

integer :: Parser Integer
integer = lexeme L.decimal

semi :: Parser String
semi = symbol ";"

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

rwords :: [String]
rwords =
    [ "true"
    , "false"
    , "skip"
    , "if"
    , "then"
    , "else"
    , "while"
    , "do"
    , "try"
    , "catch"
    ]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = liftA2 (:) letterChar (many alphaNumChar)
    check x = if x `elem` rwords
        then fail $ "keyword " ++ show x ++ " cannot be an identifier"
        else return x

-- PARSER --

whileParser :: Parser Stm
whileParser = between sc eof stmt

-- Statements

stmt :: Parser Stm
stmt = f <$> stmt' `sepBy1` semi
    where f l@(x :| xs) = if null xs then x else Seq l

stmt' :: Parser Stm
stmt' = foldr1 (<|>) [skip, ifThnEls, while, tryCatch, assignment, parens stmt]

skip :: Parser Stm
skip = Skip <$ rword "skip"

ifThnEls :: Parser Stm
ifThnEls = liftA3 IfThnEls
                  (rword "if" *> bExp)
                  (rword "then" *> stmt)
                  (rword "else" *> stmt)

while :: Parser Stm
while = liftA2 While (rword "while" *> bExp) (rword "do" *> stmt)

tryCatch :: Parser Stm
tryCatch = liftA2 TryCatch (rword "try" *> stmt) (rword "catch" *> stmt)

assignment :: Parser Stm
assignment = liftA2 Assign (identifier <* symbol ":=") aExp

-- Expressions

aExp :: Parser AExp
aExp = makeExprParser aTerm aOps
bExp :: Parser BExp
bExp = makeExprParser bTerm bOps

aTerm :: Parser AExp
aTerm = parens aExp <|> Num <$> integer <|> Var <$> identifier

bTerm :: Parser BExp
bTerm = parens bExp <|> tru <|> fls <|> rel
  where
    tru = TrueLit <$ rword "true"
    fls = FalseLit <$ rword "false"

rel :: Parser BExp
rel = do
    a1    <- aExp
    relOp <- Equals <$ symbol "=" <|> LessThanEq <$ symbol "<="
    a2    <- aExp
    return (a1 `relOp` a2)

aOps =
    [ [prefix "-" Neg, prefix "+" id]
    , [binary "*" Times, binary "/" Div]
    , [binary "+" Plus, binary "-" Minus]
    ]
bOps = [[prefix "!" Not], [binary "&&" And]]

binary name f = InfixL (f <$ symbol name)
prefix name f = Prefix (f <$ symbol name)
