{-# LANGUAGE OverloadedStrings #-}


module Variable (
    Variable (..)
) where

import Control.Applicative ((<$>))
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.Number (Number)
import Data()
import Types


instance FromJSON Variable where
    parseJSON (Object o) = do
        typ <- o .: "type" :: Parser String
        id_ <- o .: "id"
        element_type <- o .: "element_type" :: Parser String
        vals <- (rangeList o :: Parser [Number])
        let strs = [typ ++ ":" ++ (show val) | val <- vals]
        case typ of
            "range" -> return $ Variable id_ (map read strs)
            _       -> undefined
        where
            rangeList obj = do
                start <- obj .: "start"
                end <- obj .: "end"
                spacing <- obj .: "spacing"
                return $ takeWhile (<= end) $ f start spacing
            f a b = a : f (a + b) b

    parseJSON _ = error "Invalid variables definition."