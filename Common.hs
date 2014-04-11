module Common where

import Data.BitVector hiding (showHex)
import Data.Binary (encode, Binary)
import qualified Data.ByteString.Lazy as B
import Data.Word
import Numeric (showHex)


type MessageID = Word8


type CommandCode = Word8


safeBitVec :: (Integral a, Show a) => Int -> a -> BV
safeBitVec n k =
    if k > maxNat n then
        error $ "Number "
              ++ show k
              ++ " is too large to fit in "
              ++ show n
              ++ " bits."
    else
        bitVec n k


bits2bytes :: [Bool] -> [Word8]
bits2bytes bits | length bits `mod` 8 == 0 = map fromInteger (pack bits)
         | otherwise = error "Bit vector must have a multiple of 8 bits."
    where
        pack [] = []
        pack bs = nat (fromBits . take 8 $ bs) : pack (drop 8 bs)


prettyBS :: [Word8] -> String
prettyBS = concatMap (flip showHex "|")


lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs


toWords :: (Binary a) => Int -> a -> [Word8]
toWords n = lastN n . B.unpack . encode

swapBytes :: [Word8] -> [Word8]
swapBytes [] = []
swapBytes (b1:b2:bs) = (b2:b1:swapBytes bs)
swapBytes _ = error "Must be an even number of bytes to swap."