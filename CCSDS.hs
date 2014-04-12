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


packetDataLength :: (CCSDS a ) => a -> BV
packetDataLength m = bitVec 16 (l-1)
    where
        l = length (secondaryHeader m) + length (payload m)


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


header :: (CCSDS a) => a -> [Word8]
header m = bits2bytes (toBits (primaryHeader m)) ++ secondaryHeader m


packet :: (CCSDS a) => a -> [Word8]
packet m = (swapBytes (header m)) ++ payload m



packCCSDS :: (CCSDS a) => a -> [Word8]
packCCSDS m = assert checkAll (packet m)
    where
        check1 = size (primaryHeader m) == 6 * 8 --bits
        check2 = length (payload m) + length (secondaryHeader m) == fromIntegral (nat (packetDataLength m)) + 1
        checkAll = check1 && check2


showCCSDS :: (CCSDS a) => a -> String
showCCSDS = prettyBS . packCCSDS