{-# LANGUAGE OverloadedStrings #-}

module Parameter (
     Parameter
    ,packParam
) where

import Control.Applicative ((<$>))
import Data.Aeson
import Data.Aeson.Types
import Data.Int (Int8, Int16, Int32, Int64)
-- unsigned ints
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Vector (toList)
import Common


data Parameter = S   String
               | B   Bool
               | I8  Int8
               | W8  Word8
               | I16 Int16
               | W16 Word16
               | I32 Int32
               | W32 Word32
               | I64 Int64
               | W64 Word64
               | F   Double
               | D   Double
               | Arr [Parameter] deriving (Show)


packParam :: Parameter -> [Word8]
packParam (S s)   = toWords (length s) s
packParam (B b)   = toWords 1 b
packParam (I8 i)  = toWords 1 i
packParam (W8 w)  = toWords 1 w
packParam (I16 i) = swapBytes $ toWords 2 i
packParam (W16 w) = swapBytes $ toWords 2 w
packParam (I32 i) = swapBytes $ toWords 4 i
packParam (W32 w) = swapBytes $ toWords 4 w
packParam (I64 i) = swapBytes $ toWords 8 i
packParam (W64 w) = swapBytes $ toWords 8 w
packParam (F f)   = swapBytes $ toWords 4 f
packParam (D d)   = swapBytes $ toWords 8 d
packParam (Arr ps)   = concatMap packParam ps


instance FromJSON Parameter where
    parseJSON (Object o) = do
        typ <- o .: "type"
        case typ of
            "string" -> S   <$> o .: "value"
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
    parseJSON _ = undefined