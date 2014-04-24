{-# LANGUAGE OverloadedStrings #-}

module Parameter (
     Parameter (..)
    ,packParam
) where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Types
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Vector (toList)
import Numeric (showHex)
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Builder
import Foreign.Marshal.Utils (fromBool)
import qualified Data.Map as M
import qualified Data.Text as T
import Common
import Auto


data Parameter = S   String String Int
               | B   String Bool
               | I8  String Int8
               | W8  String Word8
               | I16 String Int16
               | W16 String Word16
               | I32 String Int32
               | W32 String Word32
               | I64 String Int64
               | W64 String Word64
               | F   String Float
               | D   String Double
               | Arr String [Parameter]
               | Var String


packParam :: Parameter -> Auto Parameter [Byte]
packParam (S _ s n) = return $ b2w (string7 s) ++ replicate (n-(length s)) 0
packParam (B _ b)   = return $ b2w . word8 . fromBool $ b
packParam (I8 _ i)  = return $ b2w . int8 $ i
packParam (W8 _ w)  = return $ b2w . word8 $ w
packParam (I16 _ i) = return $ b2w . int16LE $ i
packParam (W16 _ w) = return $ b2w . word16LE $ w
packParam (I32 _ i) = return $ b2w . int32LE $ i
packParam (W32 _ w) = return $ b2w . word32LE $ w
packParam (I64 _ i) = return $ b2w . int64LE $ i
packParam (W64 _ w) = return $ b2w . word64LE $ w
packParam (F _ f)   = return $ b2w . floatLE $ f
packParam (D _ d)   = return $ b2w . doubleLE $ d
packParam (Arr _ ps) = concat <$> mapM packParam ps
packParam (Var name) = envLookup name >>= packParam


b2w :: Builder -> [Byte]
b2w = B.unpack . toLazyByteString


instance FromJSON Parameter where
    parseJSON (Object o) = do
        typ <- o .: "type"
        label <- o .:? "label" .!= "unnamed"
        case typ of
            "string" -> S   label <$> o .: "value" <*> o .: "length"
            "bool"   -> B   label <$> o .: "value"
            "int8"   -> I8  label <$> o .: "value"
            "uint8"  -> W8  label <$> o .: "value"
            "int16"  -> I16 label <$> o .: "value"
            "uint16" -> W16 label <$> o .: "value"
            "int32"  -> I32 label <$> o .: "value"
            "uint32" -> W32 label <$> o .: "value"
            "int64"  -> I64 label <$> o .: "value"
            "uint64" -> W64 label <$> o .: "value"
            "float"  -> F   label <$> o .: "value"
            "double" -> D   label <$> o .: "value"
            "array"  -> do
                elemTyp <- (o .: "element_type") :: Parser String
                vals <- (o .: "values") :: Parser Array
                elems <- mapM (parseJSON . makeElem elemTyp) (toList vals)
                return $ Arr label elems
            t -> error $ "Invalid argument type: " ++ t
        where
            makeElem t v = object ["type" .= t, "value" .= v]

    parseJSON (String s) = return $ Var (T.unpack s)

    parseJSON _ = error "Invalid parameter type."


instance Show Parameter where

    show p@(S l _ _) = showParam p l
    show p@(B l _)   = showParam p l
    show p@(I8 l _)  = showParam p l
    show p@(W8 l _)  = showParam p l
    show p@(I16 l _) = showParam p l
    show p@(W16 l _) = showParam p l
    show p@(I32 l _) = showParam p l
    show p@(W32 l _) = showParam p l
    show p@(I64 l _) = showParam p l
    show p@(W64 l _) = showParam p l
    show p@(F l _)   = showParam p l
    show p@(D l _)   = showParam p l
    show p@(Arr l _) = showParam p l
    show p@(Var l) = showParam p l


showParam :: Parameter -> String -> String
showParam p l = l ++ ":FIX" -- ++ concatMap (`showHex` "|") (packParam p)