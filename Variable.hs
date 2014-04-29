{-# LANGUAGE OverloadedStrings #-}


module Variable (
    Variable (..)
) where

import Control.Applicative ((<$>))
import Data.Aeson
import Data.Aeson.Types
import Data.Vector (toList)
import Data.Scientific (fromFloatDigits, Scientific)
import System.Random
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
            "random" -> do
                dats <- map (fromParse element_type) <$> (randList o :: Parser [Value])
                return $ Variable id_ dats
            _       -> undefined
        where
            rangeList obj = do
                start <- obj .: "start"
                end <- obj .: "end"
                spacing <- obj .: "spacing"
                case (start, end, spacing) of
                    (Number s, Number e, Number sp) ->
                         return . map (Number) . takeWhile (<= e) . mkRange s $ sp
                    _          -> error "Invalid range type."
            randList obj = do
                low <- obj .: "low"
                high <- obj .: "high"
                case (low, high) of
                    (Number l, Number h) -> do
                        let rs = mkRand (sciToFloat l) (sciToFloat h) (mkStdGen 42)
                        return $ map (Number . fromFloatDigits) rs
                    _          -> error "Invalid rand type."
            mkRange a b = a : mkRange (a + b) b
            mkRand low high gen = next : mkRand low high newGen
                where
                    (next, newGen) = randomR (low, high) gen
            sciToFloat :: Scientific -> Float
            sciToFloat = fromRational . toRational

    parseJSON _ = error "Invalid variables definition."