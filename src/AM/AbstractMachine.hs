{-# LANGUAGE OverloadedLists #-}
module AM.AbstractMachine where

import qualified Data.Map                      as M
import           Data.Map                       ( (!?) )
import           Code.Inst
import           Control.Applicative            ( liftA2 )
import           Control.Arrow                  ( (>>>) )
import           Data.Bool                      ( bool )
import           Data.List.NonEmpty
import qualified Data.List.NonEmpty            as NE
import           AM.Configuration

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
    (STORE x : c, SInt z : e, s'@(status, s)) -> case (status, z) of
        (Ok, Just z') -> Just $ Conf (c, e, (status, M.insert x z' s))
        _ -> Just $ Conf (c, e, (Fail, s))
    (NOOP : c, e, s) -> Just $ Conf (c, e, s)
    (BRANCH c1 c2 : c, SBool t : e, s@(status, _)) -> case status of
        Ok -> Just $ Conf (maybe [] (bool c2 c1) t ++ c, e, s)
        Fail -> Just $ Conf (c, e, s)
    (LOOP c1 c2 : c, e, s@(status, _)) -> case status of
        Ok -> Just $ Conf (c1 ++ [BRANCH (c2 ++ [LOOP c1 c2]) [NOOP]] ++ c, e, s)
        Fail -> Just $ Conf (c, e, s)

    (CATCH c1 : c, e, (status, s)) ->
        Just $ Conf ((if status == Fail then c1 else []) ++ c, e, (Ok, s))

    (TRYCATCH c1 c2 : c, e, s@(status, _)) -> case status of
        Ok -> Just $ Conf (c1 ++ [CATCH c2] ++ c, e, s)
        Fail -> Just $ Conf (c, e, s)

    _ -> Nothing

steps :: Configuration -> NonEmpty Configuration
steps c = maybe [c] ((c <|) . steps) (step c)

mkStartConf :: Code -> Configuration
mkStartConf code = Conf (code, [], (Ok, M.empty))

executionConfigs :: Code -> NonEmpty Configuration
executionConfigs = mkStartConf >>> steps

execute :: Code -> Configuration
execute = executionConfigs >>> NE.last