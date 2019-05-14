module AM.AbstractMachine
    ( configurationGraph
    , configSteps
    , execute
    )
where

import qualified Data.Map                      as M
import           Data.Map                       ( (!) )
import           Code.Inst
import           Data.List.NonEmpty             ( NonEmpty )
import qualified Data.List.NonEmpty            as NE
import           AM.Configuration
import           Operations.Operations
import           Data.Tree
import           Data.Maybe                     ( mapMaybe )
import           Utils.Utils                    ( leafs )
import qualified Data.Set                      as Set
import qualified Control.Monad.State           as State
import           ProgramTypes.EvalWithOps

step :: Configuration i b -> EvalWithOps i b [Configuration i b]
step (Conf conf) = do
    Operations {..} <- getOps
    return $ case conf of
        (Inst _ (PUSH n) : c, e, s) -> [Conf (c, SInt (absI n) : e, s)]
        (Inst _ NEG : c, SInt z : e, s) -> [Conf (c, SInt (negate' z) : e, s)]
        (Inst _ ADD : c, SInt z1 : SInt z2 : e, s) ->
            [Conf (c, SInt (z1 `add` z2) : e, s)]
        (Inst _ MULT : c, SInt z1 : SInt z2 : e, s) ->
            [Conf (c, SInt (z1 `multiply` z2) : e, s)]
        (Inst _ DIV : c, SInt z1 : SInt z2 : e, s) ->
            [Conf (c, SInt (z1 `divide` z2) : e, s)]
        (Inst _ SUB : c, SInt z1 : SInt z2 : e, s) ->
            [Conf (c, SInt (z1 `subtr` z2) : e, s)]

        (Inst _ TRUE  : c, e, s) -> [Conf (c, SBool (absB True) : e, s)]
        (Inst _ FALSE : c, e, s) -> [Conf (c, SBool (absB False) : e, s)]
        (Inst _ EQ' : c, SInt z1 : SInt z2 : e, s) ->
            [Conf (c, SBool (z1 `eq` z2) : e, s)]
        (Inst _ LE : c, SInt z1 : SInt z2 : e, s) ->
            [Conf (c, SBool (z1 `leq` z2) : e, s)]
        (Inst _ AND : c, SBool t1 : SBool t2 : e, s) ->
            [Conf (c, SBool (t1 `and'` t2) : e, s)]
        (Inst _ NOT : c, SBool t : e, s) -> [Conf (c, SBool (neg t) : e, s)]

        (Inst _ (FETCH x) : c, e, s'@(_, s)) ->
            [Conf (c, SInt (s ! x) : e, s')]
        (Inst _ (STORE x) : c, SInt z : e, (status, s)) -> case status of
            Ok -> mconcat
                [ [ Conf (c, e, (Fail, s)) | possiblyAErr z ]
                , [ Conf (c, e, (status, M.insert x z s)) | possiblyInt z ]
                ]
            Fail -> [Conf (c, e, (Fail, s))]
        (Inst _ NOOP : c, e, s) -> [Conf (c, e, s)]
        (Inst _ (BRANCH c1 c2) : c, SBool t : e, s'@(status, s)) ->
            case status of
                Ok -> mconcat
                    [ [ Conf (c1 ++ c, e, s') | possiblyTrue t ]
                    , [ Conf (c2 ++ c, e, s') | possiblyFalse t ]
                    , [ Conf (c, e, (Fail, s)) | possiblyBErr t ]
                    ]
                Fail -> [Conf (c, e, (Fail, s))]
        (loop@(Inst cp (LOOP c1 c2)) : c, e, s@(status, _)) -> case status of
            Ok -> let unfld = Inst cp (BRANCH (c2 ++ [loop]) [Inst cp NOOP])
                  in  [Conf (c1 ++ [unfld] ++ c, e, s)]
            Fail -> [Conf (c, e, s)]

        (Inst _ (CATCH c1) : c, e, (status, s)) ->
            [Conf ((if status == Fail then c1 else []) ++ c, e, (Ok, s))]

        (Inst _ (TRYCATCH c1 c2@(Inst cpp2 _ : _)) : c, e, s@(status, _)) -> case status of
            Ok   -> [Conf (c1 ++ [Inst cpp2 (CATCH c2)] ++ c, e, s)]
            Fail -> [Conf (c, e, s)]

        _ -> []

steps :: (Ord i, Ord b) => Configuration i b -> EvalWithOps i b (Tree (Configuration i b))
steps = flip State.evalStateT Set.empty . unfoldTreeM_BF f
    where f c = do
            State.modify (c `Set.insert`)
            visited <- State.get
            next <- filter (`Set.notMember` visited) <$> State.lift (step c)
            return (c, next)

mkStartConf :: Code -> Configuration i b
mkStartConf code = Conf (code, [], (Ok, M.empty))

configurationGraph :: (Ord i, Ord b) => Code -> EvalWithOps i b (Tree (Configuration i b))
configurationGraph = steps . mkStartConf

configSteps :: (Ord i, Ord b) => Code -> EvalWithOps i b [NonEmpty (Configuration i b)]
configSteps = fmap (mapMaybe NE.nonEmpty . levels) . configurationGraph

-- | Execute the code and get all possible end configurations.
execute :: (Ord i, Ord b) => Code -> EvalWithOps i b [Configuration i b]
execute = fmap (NE.filter codeEmpty . leafs) . configurationGraph
