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
            makeRange "uint8" id_ = do
                vals <- rangeList o
                return $ Variable id_ (map W8 vals)
            makeRange "uint16" id_ = do
                vals <- rangeList o
                return $ Variable id_ (map W16 vals)
            makeRange "uint32" id_ = do
                vals <- rangeList o
                return $ Variable id_ (map W32 vals)
            makeRange "uint64" id_ = do
                vals <- rangeList o
                return $ Variable id_ (map W64 vals)
            makeRange "int8" id_ = do
                vals <- rangeList o
                return $ Variable id_ (map I8 vals)
            makeRange "int16" id_ = do
                vals <- rangeList o
                return $ Variable id_ (map I16 vals)
            makeRange "int32" id_ = do
                vals <- rangeList o
                return $ Variable id_ (map I32 vals)
            makeRange "int64" id_ = do
                vals <- rangeList o
                return $ Variable id_ (map I64 vals)
            makeRange "float" id_ = do
                vals <- rangeList o
                return $ Variable id_ (map F vals)
            makeRange "double" id_ = do
                vals <- rangeList o
                return $ Variable id_ (map D vals)
            makeRange _ id_ = undefined

            rangeList obj = do
                start <- obj .: "start"
                end <- obj .: "end"
                spacing <- obj .: "spacing"
                return $ takeWhile (<= end) $ f start spacing

            f a b = a : f (a + b) b


    parseJSON _ = error "Invalid variables definition."