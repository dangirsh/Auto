-- http://public.ccsds.org/publications/archive/133x0b1c2.pdf
-- page 4-2

module CCSDS where

import Data.BitVector
import Control.Exception.Base (assert)
import Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy as B
import Common
import Auto
import Types


packetVersionNumber :: BV
packetVersionNumber = zeros 3


secondaryHeaderFlag :: BV
secondaryHeaderFlag = fromBool True


sequenceFlags :: BV
sequenceFlags = ones 2


sequenceCount :: BV
sequenceCount = zeros 14


packetDataLength :: (CCSDS a) => a -> Auto BV
packetDataLength m = do
    pl <- length <$> payload m
    return . bitVec 16 $ length (secondaryHeader m) + pl - 1


class CCSDS a where

    packetType :: a -> BV

    applicationProcessId :: a -> BV

    secondaryHeader :: a -> [Byte]

    payload :: a -> Auto [Byte]



primaryHeader :: (CCSDS a) => a -> Auto BV
primaryHeader m = do
    dataLength <- packetDataLength m
    return $ packetVersionNumber
             # packetType m
             # secondaryHeaderFlag
             # applicationProcessId m
             # sequenceFlags
             # sequenceCount
             # dataLength


header :: (CCSDS a) => a -> Auto [Byte]
header m = do
    pri <- bits2bytes . toBits <$> primaryHeader m
    return $ pri ++ secondaryHeader m


packet :: (CCSDS a) => a -> Auto [Byte]
packet m = do
    p <- payload m
    h <- swapBytes <$> header m
    return $ h ++ p


packCCSDS :: (CCSDS a) => a -> Auto B.ByteString
packCCSDS m = do
    p <- packet m
    pri <- primaryHeader m
    pld <- payload m
    len <- fromIntegral . nat <$> packetDataLength m
    let check1 = size pri == 6 * 8 --bits
    let check2 = length pld + length (secondaryHeader m) == len + 1
    let checkAll = check1 && check2
    return $ assert checkAll (B.pack p)