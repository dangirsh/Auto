{-# LANGUAGE NamedFieldPuns, FlexibleContexts #-}


import System.Environment (getArgs)
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
import Send
import Parse
import Types
import CCSDS
import Controller()
import Message()

main :: IO ()
--main = getArgs >>= mapM_ (parseFile >=> runner)
main = (parseFile >=> runner) "main.ctrl"


myForkIO :: IO () -> IO (MVar ())
myForkIO io = do
    mvar <- newEmptyMVar
    forkFinally io (\_ -> putMVar mvar ())
    return mvar


myForkIOs :: [IO ()] -> IO ()
myForkIOs actions = mapM myForkIO actions >>= mapM_ takeMVar


runner :: Controller -> IO ()
runner (Controller {meta, sequenced, parallel}) =
    sequence_ $ mapM_ (run meta) sequenced : map (run meta) parallel
    --myForkIOs $ mapM_ (run meta) sequenced : map (run meta) parallel


run :: ControllerMeta -> MessageMeta -> IO ()
run (ControllerMeta {ip, port}) (MessageMeta {file, frequency}) = do
    packed <- uncurry zip <$> getPacked
    forM_ packed (\(bs, s) -> do
        sendUDP ip port bs
        print s
        hFlush stdout
        threadDelay . round $ 1000000 / frequency
        )
    where
        getPacked =
            case takeExtensions file of
                ".tlm" -> pack <$> (parseFile file :: IO (MessageDef Telemetry))
                ".cmd" -> pack <$> (parseFile file :: IO (MessageDef Command))
                ext      -> error $ "Unknown file extension: " ++ ext


pack :: (FromJSON a, CCSDS (Message a), AutoShow a) => MessageDef a -> ([B.ByteString], [String])
pack (MessageDef {variables=vs, message=m}) =
    (map (runAuto (packCCSDS m)) envs, map (runAuto (autoShow m)) envs)
    where
        varToPairs (Variable id_ ds) = cycle [(id_, Parameter id_ d) | d <- ds]
        envs =
            case map (Config . M.fromList) . transpose . map varToPairs $ vs of
                [] -> cycle [Config M.empty] -- hack around case for no variables
                xs -> xs
