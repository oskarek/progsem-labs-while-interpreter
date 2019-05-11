module Operations.Operations where

import           Control.Applicative            ( liftA2 )
import           Control.Monad                  ( join )
import           Data.Maybe                     ( isJust )
import           AM.Lattice              hiding ( join )

mIntDiv :: Maybe Integer -> Maybe Integer -> Maybe Integer
mIntDiv i1 i2 = join $ liftA2 safeDiv i1 i2
    where safeDiv i1' i2' = if i2' == 0 then Nothing else Just (i1' `div` i2')

instance Lattice (Maybe a)

mIntBoolOps :: Operations (Maybe Integer) (Maybe Bool)
mIntBoolOps = Operations
    { absI          = Just
    , absB          = Just
    , negate'       = fmap negate
    , add           = liftA2 (+)
    , subtr         = liftA2 (-)
    , multiply      = liftA2 (*)
    , divide        = mIntDiv
    , eq            = liftA2 (==)
    , leq           = liftA2 (<=)
    , and'          = liftA2 (&&)
    , neg           = fmap not
    , possiblyAErr  = (== Nothing)
    , possiblyBErr  = (== Nothing)
    , possiblyTrue  = (== Just True)
    , possiblyFalse = (== Just False)
    , possiblyInt   = isJust
    , isInt         = isJust
    }

data Operations i b = Operations
    { absI :: Integer -> i
    , absB :: Bool -> b
    , negate' :: i -> i
    , add :: i -> i -> i
    , subtr :: i -> i -> i
    , multiply :: i -> i -> i
    , divide :: i -> i -> i
    , eq :: i -> i -> b
    , leq :: i -> i -> b
    , and' :: b -> b -> b
    , neg :: b -> b
    , possiblyAErr :: i -> Bool
    , possiblyBErr :: b -> Bool
    , possiblyTrue :: b -> Bool
    , possiblyFalse :: b -> Bool
    , possiblyInt :: i -> Bool
    , isInt :: i -> Bool }
