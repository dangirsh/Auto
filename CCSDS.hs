-- http://public.ccsds.org/publications/archive/133x0b1c2.pdf
-- page 4-2

module CCSDS where

import Data.Word
import Data.BitVector
import Control.Exception.Base (assert)
import Common


packetVersionNumber :: BV
packetVersionNumber = zeros 3


secondaryHeaderFlag :: BV
secondaryHeaderFlag = fromBool True


sequenceFlags :: BV
sequenceFlags = ones 2


sequenceCount :: BV
sequenceCount = zeros 14


packetDataLength :: (CCSDS a) => a -> BV
packetDataLength = bitVec 16 . (+1) . length . payload


class (Show a) => CCSDS a where

    packetType :: a -> BV

    applicationProcessId :: a -> BV

    secondaryHeader :: a -> [Word8]

    payload :: a -> [Word8]



primaryHeader :: (CCSDS a) => a -> BV
primaryHeader m = packetVersionNumber
                # packetType m
                # secondaryHeaderFlag
                # applicationProcessId m
                # sequenceFlags
                # sequenceCount
                # packetDataLength m


packCCSDS :: (CCSDS a) => a -> [Word8]
packCCSDS m = assert checkAll packet
    where
    pri = primaryHeader m
    check1 = size pri == 6 * 8 --bits
    header = bits2bytes (toBits pri) ++ secondaryHeader m
    packet = (swapBytes header) ++ payload m
    check2 = length packet == fromIntegral (nat (packetDataLength m)) + length header - 1
    checkAll = check1 && check2


showCCSDS :: (CCSDS a) => a -> String
showCCSDS = prettyBS . packCCSDS