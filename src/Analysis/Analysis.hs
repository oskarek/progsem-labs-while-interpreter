module Analysis.Analysis where

import           AM.Configuration
import           Data.Tree
import qualified Utils.Utils                   as Utils
import qualified Data.List.NonEmpty            as NE
import qualified Data.Map                      as M
import           Code.Inst
import           AM.Lattice
import qualified Data.List                     as List
import qualified Data.Maybe                    as May
import           Control.Applicative            ( (<|>) )
import           Operations.Operations
import           ProgramTypes.EvalWithOps

data ControlPoint = CP Int | EndPoint deriving (Eq, Ord, Show)

data MoreLubInfo i b = CondLub b | RhsLub i
instance (Show i, Show b) => Show (MoreLubInfo i b) where
  show (CondLub b) = "cond: " ++ show b
  show (RhsLub i) = "rhs: " ++ show i

data CPState = MaybeExceptional | Normal

data LubInfo i b = LubInfo { varLubs :: M.Map String i
                           , moreInfo :: Maybe (MoreLubInfo i b)
                           , potentialExc :: Bool
                           , cpState :: CPState }
instance (Show i, Show b) => Show (LubInfo i b) where
  show LubInfo {..} = unwords $ May.catMaybes
      [Just varLubsStr, show <$> moreInfo, errorStr]
    where varLubsStr = "{" ++ List.intercalate ", " (fmap f (M.toList varLubs)) ++ "}"
          f (x, a) = x ++ "=" ++ show a
          errorStr = if potentialExc then Just "(⚠️ potential exception raiser!)" else Nothing

groupOnControlPoint
  :: Tree (Configuration i b)
  -> M.Map ControlPoint (NE.NonEmpty (Configuration i b))
groupOnControlPoint = Utils.groupedWith ctrlPoint
 where
  ctrlPoint (Conf (c, _, _)) = case c of
    []            -> EndPoint
    Inst cp _ : _ -> CP cp
    
collectLubInfo
  :: (Lattice i, Lattice b)
  => NE.NonEmpty (Configuration i b)
  -> EvalWithOps i b (Maybe (LubInfo i b))
collectLubInfo (NE.toList -> t) = do
  Operations {..} <- getOps
  let
    nonCatches       = filter (not . isCatch) t
    nonFails = May.mapMaybe nonFail nonCatches
    getMoreInfo v f = v . foldr1 lub <$> NE.nonEmpty (May.mapMaybe f nonFails)
    m = foldr (M.unionWith lub) M.empty (snd . getState <$> nonFails)
    a = getMoreInfo RhsLub getRhs
    b = getMoreInfo CondLub getCond
    e = case a of
      Just (RhsLub z) -> possiblyAErr z
      _               -> False
    cpState = if length nonFails < length nonCatches then MaybeExceptional else Normal
  return
    $ if null nonFails then Nothing else Just $ LubInfo m (a <|> b) e cpState
 where
  nonFail c = case getState c of
    (Ok  , _) -> Just c
    (Fail, _) -> Nothing

  getState (Conf (_, _, s)) = s

  getRhs (Conf (c, e, _)) = case (c, e) of
    (Inst _ (STORE _) : _, SInt z : _) -> Just z
    _ -> Nothing

  getCond (Conf (c, e, _)) = case (c, e) of
    (Inst _ (BRANCH _ _) : _, SBool t' : _) -> Just t'
    _ -> Nothing

  isCatch (Conf (c, e, _)) = case (c, e) of
    (Inst _ (CATCH _) : _, _) -> True
    _                         -> False

ctrlPointLubInfo
  :: (Lattice i, Lattice b)
  => Tree (Configuration i b)
  -> EvalWithOps i b (M.Map ControlPoint (LubInfo i b))
ctrlPointLubInfo =
  M.traverseMaybeWithKey (const collectLubInfo) . groupOnControlPoint
