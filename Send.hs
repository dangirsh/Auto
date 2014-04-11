module Send (
  sendUDP
) where

import Control.Applicative
import Network.Socket hiding (sendTo)
import Network.Socket.ByteString (sendTo)
import qualified Data.ByteString.Lazy as B


--main :: IO ()
--main = do
--  n <- sendUDP "127.0.0.1" 1234 (pack [65, 65, 65, 65, 65])
--  print n


sendUDP :: String -> Integer -> B.ByteString -> IO ()
sendUDP ip port dat = do
  sock <- socket AF_INET Datagram defaultProtocol
  addr <- SockAddrInet (fromInteger port) <$> inet_addr ip
  _ <- sendTo sock (B.toStrict dat) addr
  sClose sock
  return ()