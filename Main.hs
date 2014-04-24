import System.Environment (getArgs)
import Control.Monad
import Control.Applicative
import Data.BitVector hiding (showHex)
import Numeric (showHex)
import qualified Data.ByteString.Lazy as B
import Control.Concurrent
import Control.Concurrent.MVar
import Controller
import Send
import Parse


main :: IO ()
--main = mapM (parseFile >=> run) <$> getArgs
main = (parseFile >=> run) "main.ctrl"


myForkIO :: IO () -> IO (MVar ())
myForkIO io = do
    mvar <- newEmptyMVar
    forkFinally io (\_ -> putMVar mvar ())
    return mvar


myForkIOs :: [IO ()] -> IO ()
myForkIOs actions = mapM myForkIO actions >>= mapM_ takeMVar


run :: Controller -> IO ()
run (Controller {meta=cm, sequenced=s, parallel=p}) = do
    let actions = [mapM_ go s] ++ map go p
    --myForkIOs actions
    sequence_ actions
    where
        go mm = send cm (frequency mm) (file mm) >> return ()