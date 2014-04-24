{-# LANGUAGE OverloadedStrings #-}


module Variable (
    Variable (..)
) where

import Data.Aeson
import Data.Aeson.Types
import Types


instance FromJSON Variable where
    parseJSON (Object o) = do
        typ <- o .: "type" :: Parser String
        id_ <- o .: "id"
        element_type <- o .: "element_type" :: Parser String
        case typ of
            "range" -> makeRange element_type id_
            _       -> undefined
        where
            makeRange "float" id_ = do
                vals <- rangeList o
                return $ Variable id_ (map F vals)
            makeRange "uint8" id_ = do
                vals <- rangeList o
                return $ Variable id_ (map W8 vals)
            makeRange _ id_ = undefined

            rangeList obj = do
                start <- obj .: "start"
                end <- obj .: "end"
                spacing <- obj .: "spacing"
                return $ takeWhile (<= end) $ f start spacing

            f a b = a : f (a + b) b


    parseJSON _ = error "Invalid variables definition."