{-# LANGUAGE OverloadedStrings #-}


module Variable (
    Variable (..)
) where

import Control.Applicative ((<$>))
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.Number (Number)
import Data.Vector (toList)
import Data (fromParse)
import Types


instance FromJSON Variable where
    parseJSON (Object o) = do
        typ <- o .: "type" :: Parser String
        id_ <- o .: "id"
        element_type <- o .: "element_type" :: Parser String
        case typ of
            "cycle" -> do
                dats <- map (fromParse element_type) <$> (rangeList o :: Parser [Value])
                return $ Variable id_ (cycle dats)
            "sequence" -> do
                vals <- toList <$> (o .: "values" :: Parser Array)
                let dats = map (fromParse element_type) vals
                return $ Variable id_ (cycle dats)
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