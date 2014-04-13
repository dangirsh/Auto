{-# LANGUAGE DeriveGeneric #-}

module Controller where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)


data Controller = Controller {
     sequenced :: [MessageMeta]
    ,parallel :: [MessageMeta]
} deriving (Show, Generic)

instance FromJSON Controller


data MessageMeta = MessageMeta {
     file :: FilePath
    ,frequency :: Double
    ,repetitions :: Int
} deriving (Show, Generic)

instance FromJSON MessageMeta
