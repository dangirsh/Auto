{-# LANGUAGE OverloadedStrings #-}


module Variable (
    Variable (..)
) where

--import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Aeson
import Data.Aeson.Types
import Control.Applicative ((<$>))


data Variable = F String [Float]
              | W8 String [Word8] deriving (Show)


instance FromJSON Variable where
    parseJSON (Object o) = do
        typ <- o .: "type" :: Parser String
        id_ <- o .: "id"
        element_type <- o .: "element_type" :: Parser String
        case typ of
            "range" -> makeRange element_type id_
            _       -> undefined
        where
            makeRange "float" id_ = F id_ <$> rangeList o
            makeRange "uint8" id_ = W8 id_ <$> rangeList o
            makeRange _ id_ = undefined

            rangeList obj = do
                start <- obj .: "start"
                end <- obj .: "end"
                spacing <- obj .: "spacing"
                return $ takeWhile (<= end) $ f start spacing

            f a b = a : f (a + b) b


    parseJSON _ = error "Invalid variables definition."