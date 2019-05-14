module AM.Configuration where

import           Code.Inst
import           Data.Map                       ( Map
                                                , toList
                                                )
import           Data.List                      ( intercalate )

data StackVal i b = SInt i | SBool b deriving (Eq, Ord, Show)
type Stack i b = [StackVal i b]
data Status = Ok | Fail deriving (Eq, Ord, Show)
type State i = (Status, Map String i)
newtype Configuration i b = Conf (Code, Stack i b, State i) deriving (Eq, Show, Ord)

codeEmpty :: Configuration i b -> Bool
codeEmpty (Conf (c, _, _)) = null c

showStackVal :: (Show i, Show b) => StackVal i b -> String
showStackVal v = case v of
    SInt z -> show z
    SBool t -> show t

showConfig :: (Show i, Show b) => Configuration i b -> String
showConfig (Conf (c, e, (status, s))) = 
    unlines [ "Code: " ++ disp' (map show c)
            , "Stack: " ++ disp' (map showStackVal e)
            , "State: " ++ sSym ++ disp ", " "[" "]" (map mapsTo $ toList s) ]
    where sSym = case status of Ok -> "✅ "; Fail -> "🚩 "
          mapsTo (x, a) = x ++ " ⟼  " ++ show a
          disp' = disp ":" "" ""
          disp sep b e' x = if null x then "ε" else b ++ intercalate sep x ++ e'