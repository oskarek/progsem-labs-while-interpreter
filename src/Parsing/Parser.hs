module Parsing.Parser
    ( whileParser
    )
where

import           Syntax.AST
import           Data.Functor                   ( (<$) )
import           Data.Void
import           Control.Applicative            ( empty
                                                , liftA2
                                                )
import           Control.Monad.Combinators.Expr
import           Text.Megaparsec         hiding ( sepBy1 )
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import           Control.Applicative.Combinators.NonEmpty
                                                ( sepBy1 )
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified Control.Monad.State           as State

type Parser = ParsecT Void String (State.State Int)

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
stmt = do
    cp <- State.get
    f cp <$> stmt' `sepBy1` semi
    where f cp l@(x :| xs) = if null xs then x else Stm cp (Seq l)

stmt' :: Parser Stm
stmt' = foldr1 (<|>) [skip, ifThnEls, while, tryCatch, assignment, parens stmt]

skip :: Parser Stm
skip = do
    cp <- State.get
    sk <- Stm cp Skip <$ rword "skip"
    State.modify (+1)
    return sk

ifThnEls :: Parser Stm
ifThnEls = do
    cp <- State.get
    cond <- rword "if" >> State.modify (+1) >> bExp
    thn <- rword "then" *> stmt
    els <- rword "else" *> stmt
    return (Stm cp (IfThnEls cond thn els))

while :: Parser Stm
while = do
    cp <- State.get
    wh <- rword "while" >> State.modify (+1) >> bExp
    do' <- rword "do" *> stmt
    return (Stm cp (While wh do'))

tryCatch :: Parser Stm
tryCatch = do
    cp <- State.get
    t <- rword "try" >> State.modify (+1) >> stmt
    c <- rword "catch" *> stmt
    return (Stm cp (TryCatch t c))

assignment :: Parser Stm
assignment = do
    cp <- State.get
    var <- identifier <* symbol ":="
    State.modify (+1)
    Stm cp . Assign var <$> aExp

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
