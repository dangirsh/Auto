{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module Send (
    send
) where


import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Applicative
import Network.Socket hiding (sendTo, send)
import Network.Socket.ByteString (sendTo)
import qualified Data.ByteString.Lazy as B
import System.FilePath.Posix (takeExtensions)
import Control.Concurrent (threadDelay)
import Text.Show.Pretty (ppShow)
import System.IO
import qualified Data.Map as M
import Data.List (transpose)
import Data.Aeson (FromJSON)
import Controller
import Common
import Parse
import CCSDS
import Command
import Telemetry
import Parameter
import Message
import qualified Variable as V
import Auto


send :: ControllerMeta -> Frequency -> FilePath -> IO ()
send meta freq file =
    case takeExtensions file of
        ".tlm" -> void (sendFile meta freq file :: IO (Message Telemetry))
        ".cmd" -> void (sendFile meta freq file :: IO (Message Command))


sendFile :: (FromJSON a, CCSDS (Message a)) => ControllerMeta -> Frequency -> FilePath -> IO (Message a)
sendFile meta freq file = do
    (MessageDef {variables=vs, message=m}) <- getFile
    mapM_ (runAuto (sendCCSDS meta freq m)) $ makeEnvs vs
    return m
    where
        --getFile :: (FromJSON a) => IO (MessageDef a)
        getFile = parseFile file
        varToPairs (V.F id_ vals) = [(id_, F id_ val) | val <- vals]
        varToPairs (V.W8 id_ vals) = [(id_, W8 id_ val) | val <- vals]
        --varToPairs _  = undefined
        makeEnvs vs =
            let jaggedPairs = map varToPairs vs in
            let smallestLen = minimum . map length $ jaggedPairs in
            let flushPairs = map (take smallestLen) jaggedPairs in
            map M.fromList $ transpose flushPairs


sendCCSDS :: (CCSDS a) => ControllerMeta -> Frequency -> a -> Auto Parameter ()
sendCCSDS (ControllerMeta {ip=ip, port=port}) freq ccsds = do
    packed <- packCCSDS ccsds
    liftIO $ sendUDP ip port . B.pack $ packed
    mapM_ liftIO $ [putStrLn $ ppShow ccsds
                   ,hFlush stdout
                   ,threadDelay . round $ 1000000 / freq]


sendUDP :: String -> Integer -> B.ByteString -> IO ()
sendUDP ip port dat = do
    sock <- socket AF_INET Datagram defaultProtocol
    addr <- SockAddrInet (fromInteger port) <$> inet_addr ip
    _ <- sendTo sock (B.toStrict dat) addr
    sClose sock
    return ()