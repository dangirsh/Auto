module Send (
    sendUDP
) where

import Control.Applicative
import Network.Socket hiding (sendTo)
import Network.Socket.ByteString (sendTo)
import qualified Data.ByteString.Lazy as B


sendUDP :: String -> Integer -> B.ByteString -> IO ()
sendUDP ip port dat = do
    sock <- socket AF_INET Datagram defaultProtocol
    addr <- SockAddrInet (fromInteger port) <$> inet_addr ip
    _ <- sendTo sock (B.toStrict dat) addr
    sClose sock
    return ()