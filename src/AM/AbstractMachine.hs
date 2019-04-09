{-# LANGUAGE OverloadedLists #-}
module AM.AbstractMachine where

import qualified Data.Map                      as M
import           Data.Map                       ( (!?)
                                                , Map
                                                )
import           Code.Inst
import           Control.Applicative            ( liftA2 )
import           Data.Bool                      ( bool )
import           Data.Maybe                     ( fromMaybe )
import           Control.Monad                  ( join )
import           Data.List.NonEmpty
import  qualified         Data.List.NonEmpty as NE

data StackVal = SInt (Maybe Integer) | SBool (Maybe Bool) deriving (Eq, Show)
type Stack = [StackVal]
data Status = Ok | Fail deriving (Eq, Show)
type State = (Status, Map String Integer)
newtype Configuration = Conf (Code, Stack, State)

instance Show Configuration where
    show (Conf (c, e, s)) =
        "Code: " ++ show c ++ "\n"
            ++ "Stack: " ++ show e ++ "\n"
            ++ "State: " ++ show s ++ "\n"

mkStartConf :: Code -> Configuration
mkStartConf code = Conf (code, [], (Ok, M.empty))

step :: Configuration -> Maybe Configuration
step (Conf conf) = case conf of
    (PUSH n : c, e, s) -> Just $ Conf (c, SInt (Just n) : e, s)
    (NEG : c, SInt z : e, s) -> Just $ Conf (c, SInt (negate <$> z) : e, s)
    (ADD : c, SInt z1 : SInt z2 : e, s) -> Just $ Conf (c, SInt (liftA2 (+) z1 z2) : e, s)
    (MULT : c, SInt z1 : SInt z2 : e, s) -> Just $ Conf (c, SInt (liftA2 (*) z1 z2) : e, s)
    (DIV : c, SInt z1 : SInt z2 : e, s) ->
        let z' = if z2 == Just 0 then Nothing else liftA2 div z1 z2
        in Just $ Conf (c, SInt z' : e, s)
    (SUB : c, SInt z1 : SInt z2 : e, s) -> Just $ Conf (c, SInt (liftA2 (-) z1 z2) : e, s)

    (TRUE : c, e, s) -> Just $ Conf (c, SBool (Just True) : e, s)
    (FALSE : c, e, s) -> Just $ Conf (c, SBool (Just False) : e, s)
    (EQ' : c, SInt z1 : SInt z2 : e, s) -> Just $ Conf (c, SBool (liftA2 (==) z1 z2) : e, s)
    (LE : c, SInt z1 : SInt z2 : e, s) -> Just $ Conf (c, SBool (liftA2 (<=) z1 z2) : e, s)
    (AND : c, SBool t1 : SBool t2 : e, s) -> Just $ Conf (c, SBool (liftA2 (&&) t1 t2) : e, s)
    (NOT : c, SBool t : e, s) -> Just $ Conf (c, SBool (not <$> t) : e, s)

    (FETCH x : c, e, s'@(status, s)) -> Just $ Conf (c, SInt (s !? x) : e, s')
    (STORE x : c, SInt z : e, (status, s)) -> case z of
        Nothing -> Just $ Conf (c, e, (Fail, s))
        Just z' -> Just $ Conf (c, e, (status, M.insert x z' s))
    (NOOP : c, e, s) -> Just $ Conf (c, e, s)
    (BRANCH c1 c2 : c, SBool t : e, s) -> Just $ Conf (maybe [] (bool c2 c1) t ++ c, e, s)
    (LOOP c1 c2 : c, e, s) ->
        Just $ Conf (c1 ++ [SBRANCH [BRANCH (c2 ++ [LOOP c1 c2]) [NOOP]] [NOOP]] ++ c, e, s)

    (SBRANCH c1 c2 : c, e, s'@(status, s)) ->
        Just $ Conf ((if status == Ok then c1 else c2) ++ c, e, s')
    (SETOKSTATE : c, e, (_, s)) -> Just $ Conf (c, e, (Ok, s))

    _ -> Nothing

steps :: Configuration -> NonEmpty Configuration
steps c = maybe [c] ((c <|) . steps) (step c)

finalConf :: Configuration -> Configuration
finalConf = NE.last . steps