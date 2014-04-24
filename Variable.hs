{-# LANGUAGE OverloadedStrings #-}


module Variable (
    Variable (..)
) where

import Control.Applicative ((<$>))
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.Number (Number)
import Data (fromParse)
import Types


instance FromJSON Variable where
    parseJSON (Object o) = do
        typ <- o .: "type" :: Parser String
        id_ <- o .: "id"
        element_type <- o .: "element_type" :: Parser String
        nums <- (rangeList o :: Parser [Value])
        let dats = map (fromParse typ) nums
        case typ of
            "range" -> return $ Variable id_ dats
            _       -> undefined
        where
            rangeList obj = do
                start <- obj .: "start"
                end <- obj .: "end"
                spacing <- obj .: "spacing"
                case (start, end, spacing) of
                    (Number s, Number e, Number sp) -> return $ map Number $ takeWhile (<= e) $ f s sp
                    _          -> error "Invalid range type."
            f a b = a : f (a + b) b

    parseJSON _ = error "Invalid variables definition."