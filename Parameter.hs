{-# LANGUAGE OverloadedStrings #-}

module Parameter (
     Parameter (..)
    ,packParam
) where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson
import Numeric (showHex)
import qualified Data.Text as T
import Types
import Data
import Auto


packParam :: Parameter -> Auto [Byte]
packParam (Parameter _ (Var name)) = envLookup name >>= packParam
packParam (Parameter _ d) = return . packData $ d


instance FromJSON Parameter where

    parseJSON (Object o) = Parameter <$> o .:? "label" .!= "unnamed"<*> parseJSON (Object o)

    parseJSON (String s) = return $ Parameter (T.unpack s) (Var (T.unpack s))

    parseJSON _ = error "Invalid parameter type."


instance AutoShow Parameter where

    autoShow p@(Parameter s _) = do
        packed <- packParam p
        return $ s ++ ":" ++ concatMap (`showHex` "|") packed
