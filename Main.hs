{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

--import System.Environment (getArgs)
import Control.Monad
import Control.Applicative
import Control.Concurrent (threadDelay)
import Data.Aeson (FromJSON, eitherDecode)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import System.IO
import Message
import CCSDS
import Send


data MessageFile = MessageFile {
     meta :: MetaData
    ,messages :: [Message]
} deriving (Show, Generic)

instance FromJSON MessageFile


data MetaData = MetaData {
     name :: String
    ,frequency :: Double
    ,repetitions :: Int
} deriving (Show, Generic)

instance FromJSON MetaData


main :: IO ()
--main = getArgs >>= mapM parseFile >>= mapM_ sendMessages
main = mapM parseFile ["test.json"] >>= mapM_ sendMessages


parseFile :: String -> IO MessageFile
parseFile file = do
    d <- eitherDecode <$> C.filter (/= '\n') <$> B.readFile file
    return $ case d of
        Right mf -> mf
        Left  m -> error m


sendMessages :: MessageFile -> IO ()
sendMessages (MessageFile {
                 meta=(MetaData {
                     name=n
                    ,frequency=freq
                    ,repetitions=r
                })
                ,messages=ms
             }) = do
    print $ "Sending " ++ n ++ " sequence "
          ++ "at " ++ show freq ++ "Hz "
          ++ show r ++ " times..."
    replicateM_ r $ mapM_ (send freq) ms


type Frequency = Double


send :: Frequency -> Message -> IO ()
send freq (TlmMessage t) = sendCCSDS freq t
send freq (CmdMessage c) = sendCCSDS freq c


sendCCSDS :: (CCSDS a) => Frequency -> a -> IO ()
sendCCSDS freq ccsds = do
    sendUDP "127.0.0.1" 1234 $ B.pack (packCCSDS ccsds)
    print ccsds
    hFlush stdout
    threadDelay (round $ 1000000 / freq)
