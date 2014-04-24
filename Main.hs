{-# LANGUAGE NamedFieldPuns, FlexibleContexts #-}


--import System.Environment (getArgs)
import Control.Monad
import Control.Applicative ((<$>))
import Control.Concurrent
import System.FilePath.Posix (takeExtensions)
import System.IO (hFlush, stdout)
import Data.Aeson (FromJSON)
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import Data.List (transpose)
import Auto
import Controller
import Send
import Parse
import Types
import CCSDS
import Message


main :: IO ()
--main = mapM (parseFile >=> run) <$> getArgs
main = (parseFile >=> runner) "main.ctrl"


myForkIO :: IO () -> IO (MVar ())
myForkIO io = do
    mvar <- newEmptyMVar
    forkFinally io (\_ -> putMVar mvar ())
    return mvar


myForkIOs :: [IO ()] -> IO ()
myForkIOs actions = mapM myForkIO actions >>= mapM_ takeMVar


runner :: Controller -> IO ()
runner (Controller {meta=cm, sequenced=s, parallel=p}) =
    --myForkIOs actions
    sequence_ actions
    where
        actions = mapM_ (run cm) s : map (run cm) p


run :: ControllerMeta -> MessageMeta -> IO ()
run (ControllerMeta {ip, port}) (MessageMeta {file, frequency}) = do
    packed <- getPacked
    forM_ packed $ (\p -> do
        sendUDP ip port p
        print p
        hFlush stdout
        threadDelay . round $ 1000000 / frequency
        )
    where
        getPacked =
            case takeExtensions file of
                ".tlm" -> pack <$> (parseFile file :: IO (MessageDef Telemetry))
                ".cmd" -> pack <$> (parseFile file :: IO (MessageDef Command))


pack :: (FromJSON a, CCSDS (Message a)) => MessageDef a -> [B.ByteString]
pack (MessageDef {variables=vs, message=m}) = do
    map (runAuto (packCCSDS m)) $ makeEnvs vs
    where
        varToPairs (Variable id_ ds) = [(id_, Parameter id_ d) | d <- ds]
        makeEnvs vs =
            let jaggedPairs = map varToPairs vs in
            let smallestLen = minimum . map length $ jaggedPairs in
            let flushPairs = map (take smallestLen) jaggedPairs in
            map Config . map M.fromList . transpose $ flushPairs