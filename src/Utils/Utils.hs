{-# LANGUAGE OverloadedLists #-}
module Utils.Utils where

import qualified Data.Foldable                 as F
import qualified Data.List.NonEmpty            as NE
import           Data.Tree                      ( Tree(..) )
import qualified Data.Semigroup                as SG
import qualified Data.Map                      as Map

showList :: Foldable t => (a -> String) -> t a -> String
showList showF (NE.nonEmpty . F.toList -> Just xs) =
  "[" ++ F.fold (NE.intersperse ", " (showF <$> xs)) ++ "]"
showList _ _ = "[]"

leafs :: Tree a -> NE.NonEmpty a
leafs (Node _    (NE.nonEmpty -> Just ts)) = SG.sconcat (leafs <$> ts)
leafs (Node root _                       ) = [root]

groupedWith
  :: (Foldable t, Ord b) => (a -> b) -> t a -> Map.Map b (NE.NonEmpty a)
groupedWith f = Map.fromAscList . fmap g . NE.groupAllWith f . F.toList
  where g c@(x NE.:| _) = (f x, c)
