module AM.Configuration where

import           Code.Inst
import           Data.Map                       ( Map, toList )
import           Data.List                      ( intercalate )

data StackVal = SInt (Maybe Integer) | SBool (Maybe Bool) deriving Eq
instance Show StackVal where
    show v = case v of
        SInt Nothing -> "⊥"
        SBool Nothing -> "⊥"
        SInt (Just z) -> show z
        SBool (Just t) -> if t then "tt" else "ff"
type Stack = [StackVal]
data Status = Ok | Fail deriving (Eq, Show)
type State = (Status, Map String Integer)
newtype Configuration = Conf (Code, Stack, State)

instance Show Configuration where
    show (Conf (c, e, (status, s))) =
        unlines [ "Code: " ++ disp' (map show c)
                , "Stack: " ++ disp' (map show e)
                , "State: " ++ sSym ++ disp ", " "[" "]" (map mapsTo $ toList s) ]
        where sSym = case status of Ok -> "✅ "; Fail -> "🚩 "
              mapsTo (x, a) = x ++ " ⟼  " ++ show a
              disp' = disp ":" "" ""
              disp sep b e x = if null x then "ε" else b ++ intercalate sep x ++ e