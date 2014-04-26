{-# LANGUAGE NamedFieldPuns, FlexibleContexts #-}


import System.Environment (getArgs)
import Control.Monad
import Control.Applicative ((<$>))
import Control.Concurrent.Async
import Control.Concurrent (threadDelay)
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
main = getArgs >>= mapM_ (parseFile >=> runner)
--main = (parseFile >=> runner) "main.ctrl"


runner :: Controller -> IO ()
runner (Controller {meta, sequenced, parallel}) = do
    s <- async $ mapM_ (run meta) sequenced
    _ <- mapConcurrently (run meta) parallel
    wait s


run :: ControllerMeta -> MessageMeta -> IO ()
run (ControllerMeta {ip, port}) (MessageMeta {file, frequency, times}) = do
    packed <- getPacked
    putStrLn $ "Starting to send " ++ file ++ " at " ++ (show frequency) ++ "Hz " ++ (show times) ++ " times.\n"
    forM_ (take times packed) (\(bs, s) -> do
        sendUDP ip port bs
        putStrLn $ file ++ ": " ++ s ++ "\n"
        hFlush stdout
        threadDelay . round $ 1000000 / frequency
        )
    where
        getPacked =
            case takeExtensions file of
                ".tlm" -> pack <$> (parseFile file :: IO (MessageDef Telemetry))
                ".cmd" -> pack <$> (parseFile file :: IO (MessageDef Command))
                ext      -> error $ "Unknown file extension: " ++ ext


pack :: (FromJSON a, CCSDS (Message a), AutoShow a) => MessageDef a -> [(B.ByteString, String)]
pack (MessageDef {variables=vs, message=m}) = zip (f packCCSDS) (f autoShow)
    where
        f g = map (runAuto (g m)) envs
        varToPairs (Variable id_ ds) = [(id_, Parameter id_ d) | d <- ds]
        envs =
            case map (Config . M.fromList) . transpose . map varToPairs $ vs of
                [] -> [Config M.empty] -- hack around case for no variables
                xs -> xs
