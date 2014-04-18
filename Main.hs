{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import System.Environment (getArgs)
import Control.Monad
import Control.Applicative
import Control.Concurrent (threadDelay)
import Data.Aeson (FromJSON, eitherDecode)
import Data.BitVector hiding (showHex)
import Numeric (showHex)
import Text.Show.Pretty
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import System.IO
import Control.Concurrent
import Control.Concurrent.MVar
import Controller
import Message
import CCSDS
import Send


main :: IO ()
--main = getArgs >>= mapM parseFile >>= mapM_ run
main = mapM parseFile ["main.ctrl"] >>= mapM_ run


parseFile :: (FromJSON a) => String -> IO a
parseFile file = do
    -- d <- eitherDecode <$> cleanFile file
    d <- eitherDecode <$> C.filter (/= '\n') <$> B.readFile file
    return $ case d of
        Right ctrl -> ctrl
        Left  e -> error e


myForkIO :: IO () -> IO (MVar ())
myForkIO io = do
    mvar <- newEmptyMVar
    forkFinally io (\_ -> putMVar mvar ())
    return mvar


myForkIOs :: [IO ()] -> IO ()
myForkIOs actions = mapM myForkIO actions >>= mapM_ takeMVar


run :: Controller -> IO ()
run (Controller {meta=m, sequenced=s, parallel=p}) = do
    let actions = [forM_ s runRepeat] ++ map runRepeat p
    myForkIOs actions
    where
        runRepeat = do
            f <- file
            freq <- frequency
            rep <- repetitions
            return $  parseFile f >>= replicateM_ rep . send m freq


type Frequency = Double


send :: ControllerMeta -> Frequency -> Message -> IO ()
send m freq (TlmMessage t) = sendCCSDS m freq t
send m freq (CmdMessage c) = sendCCSDS m freq c


sendCCSDS :: (CCSDS a) => ControllerMeta -> Frequency -> a -> IO ()
sendCCSDS (ControllerMeta {ip=ip, port=port}) freq ccsds = do
    sendUDP ip port . B.pack . packCCSDS $ ccsds
    putStrLn $ ppShow ccsds
    --print $ ((`showHex` "") . nat . applicationProcessId) ccsds
    hFlush stdout
    threadDelay (round $ 1000000 / freq)


--print $ "Sending " ++ n ++ " sequence "
--  ++ "at " ++ show freq ++ "Hz "
--  ++ show r ++ " times..."

--cleanFile :: FilePath -> IO ByteString
--cleanFile file =
--    ls <- lines <$> readFile file
--    return . B.pack . removeNewlines . removeComments $ ls
