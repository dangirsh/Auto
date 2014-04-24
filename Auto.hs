module Auto where

import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Applicative ((<$>))
import qualified Data.Map as M

type Env a = (M.Map String a)


envLookup :: (Show a) => String -> Auto a a
envLookup key = do
    maybeVal <- M.lookup key <$> ask
    case maybeVal of
        Just v -> return v
        Nothing -> error $ "Undefined environment variable: " ++ key


type Auto a = ReaderT (Env a) IO


runAuto :: Auto a b -> Env a -> IO b
runAuto = runReaderT
