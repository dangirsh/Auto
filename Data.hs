{-# LANGUAGE OverloadedStrings #-}

module Data where

import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Builder
import Data.Vector (toList)
import Foreign.Marshal.Utils (fromBool)
import Control.Applicative ((<$>), (<*>))
import Data.Aeson
import Data.Aeson.Types
import Types


packData :: Data -> [Byte]
packData (S s n) = (b2w (string7 s)) ++ replicate (n - length s) 0
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
            "string" -> S   <$> o .: "value" <*> o .: "length"
            "bool"   -> B   <$> o .: "value"
            "int8"   -> I8  <$> o .: "value"
            "uint8"  -> W8  <$> o .: "value"
            "int16"  -> I16 <$> o .: "value"
            "uint16" -> W16 <$> o .: "value"
            "int32"  -> I32 <$> o .: "value"
            "uint32" -> W32 <$> o .: "value"
            "int64"  -> I64 <$> o .: "value"
            "uint64" -> W64 <$> o .: "value"
            "float"  -> F   <$> o .: "value"
            "double" -> D   <$> o .: "value"
            "array"  -> do
                elemTyp <- (o .: "element_type") :: Parser String
                vals <- (o .: "values") :: Parser Array
                elems <- mapM (parseJSON . makeElem elemTyp) (toList vals)
                return $ Arr elems
            t -> error $ "Invalid argument type: " ++ t
        where
            makeElem t v = object ["type" .= t, "value" .= v]

    parseJSON _ = error "Invalid message definition."