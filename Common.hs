module Common where

import Data.BitVector hiding (showHex)
import Data.Word


type Byte = Word8

type Frequency = Double


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


bits2bytes :: [Bool] -> [Byte]
bits2bytes bits | length bits `mod` 8 == 0 = map fromInteger (pack bits)
         | otherwise = error "Bit vector must have a multiple of 8 bits."
    where
        pack [] = []
        pack bs = nat (fromBits . take 8 $ bs) : pack (drop 8 bs)


swapBytes :: [Byte] -> [Byte]
swapBytes [] = []
swapBytes (b1:b2:bs) = b2:b1:swapBytes bs
swapBytes _ = error "Must be an even number of bytes to swap."
