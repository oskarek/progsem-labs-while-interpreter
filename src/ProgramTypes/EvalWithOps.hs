module ProgramTypes.EvalWithOps where

import           Control.Monad.IO.Class         ( MonadIO )
import qualified Control.Monad.Reader          as R
import           Operations.Operations          ( Operations )

newtype EvalWithOps i b a = WithOps { unOps :: WithOps i b a }
    deriving (Functor, Applicative, Monad, R.MonadReader (Operations i b), MonadIO)

type WithOps i b = R.ReaderT (Operations i b) IO

getOps :: EvalWithOps i b (Operations i b)
getOps = R.ask
