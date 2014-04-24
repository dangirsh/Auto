{-# LANGUAGE DeriveGeneric #-}

module Controller where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)


data Controller = Controller {
     meta :: ControllerMeta
    ,sequenced :: [MessageMeta]
    ,parallel :: [MessageMeta]
} deriving (Show, Generic)

instance FromJSON Controller


data ControllerMeta = ControllerMeta {
     ip :: String
    ,port :: Integer
} deriving (Show, Generic)

instance FromJSON ControllerMeta


data MessageMeta = MessageMeta {
     file :: FilePath
    ,frequency :: Double
} deriving (Show, Generic)

instance FromJSON MessageMeta

