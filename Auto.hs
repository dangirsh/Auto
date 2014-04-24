module Auto where

import Control.Monad.Reader (Reader, runReader, asks)
import Control.Applicative ((<$>))
import qualified Data.Map as M
import Types


envLookup :: String -> Auto Parameter
envLookup key = do
    maybeVal <- M.lookup key <$> asks envC
    case maybeVal of
        Just v -> return v
        Nothing -> error $ "Undefined environment variable: " ++ key


type Auto = Reader Config


runAuto :: Auto a -> Config -> a
runAuto = runReader


class AutoShow a where

    autoShow :: a -> Auto String