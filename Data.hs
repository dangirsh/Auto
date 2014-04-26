{-# LANGUAGE OverloadedStrings #-}

module Data where

import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Builder
import Data.Vector (toList)
import Foreign.Marshal.Utils (fromBool)
import Control.Applicative ((<$>))
import Data.Aeson
import Data.Aeson.Types
import Data.List.Split
import Types
--import Debug.Trace (trace)
import qualified Data.Text as T


packData :: Data -> [Byte]
packData (S (s, n)) = b2w (string7 s) ++ replicate (n - length s) 0
packData (B b)   =  b2w . word8 . fromBool $ b
packData (I8 i)  =  b2w . int8 $ i
packData (W8 w)  =  b2w . word8 $ w
packData (I16 i) =  b2w . int16LE $ i
packData (W16 w) =  b2w . word16LE $ w
packData (I32 i) =  b2w . int32LE $ i
packData (W32 w) =  b2w . word32LE $ w
packData (I64 i) =  b2w . int64LE $ i
packData (W64 w) =  b2w . word64LE $ w
packData (F f)   =  b2w . floatLE $ f
packData (D d)   =  b2w . doubleLE $ d
packData (Arr a) =  concatMap packData a
packData _ = error "Invalid data type for packData."


b2w :: Builder -> [Byte]
b2w = B.unpack . toLazyByteString


instance FromJSON Data where

    parseJSON (Object o) = do
        typ <- o .: "type"
        case typ of
            "array"  -> do
                elemTyp <- (o .: "element_type") :: Parser String
                vals <- (o .: "values") :: Parser Array
                elems <- mapM (parseJSON . makeElem elemTyp) (toList vals)
                return $ Arr elems
            "string" -> do
                (Number len) <- o .: "length" :: Parser Value
                val <- o .: "value" :: Parser Value
                return $ fromParse (typ ++ ":" ++ show len) val
            _ -> fromParse typ <$> o .: "value"
        where
            makeElem t v = object ["type" .= t, "value" .= v]

    parseJSON _ = error "Invalid message definition."


fromParse :: String -> Value -> Data
fromParse "bool"   (Bool b)   = B b
fromParse "float"  (Number f) = F   (read . show $ f)
fromParse "double" (Number d) = D   (read . show $ d)
fromParse "uint8"  (Number u) = W8  (read . show . round $ u)
fromParse "uint16" (Number u) = W16 (read . show . round $ u)
fromParse "uint32" (Number u) = W32 (read . show . round $ u)
fromParse "uint64" (Number u) = W64 (read . show . round $ u)
fromParse "int8"   (Number i) = I8  (read . show . round $ i)
fromParse "int16"  (Number i) = I16 (read . show . round $ i)
fromParse "int32"  (Number i) = I32 (read . show . round $ i)
fromParse "int64"  (Number i) = I64 (read . show . round $ i)
--fromParse "array"  (Array  a) = Arr (read . show . round $ i)
fromParse x (String s) = let [_, len] = splitOn ":" x in S (T.unpack s, round . read $ len)
fromParse x _ = error $ "Invalid data type: " ++ x