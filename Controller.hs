{-# LANGUAGE DeriveGeneric #-}

module Controller where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import Types


instance FromJSON ControllerMeta

instance FromJSON Controller

instance FromJSON MessageMeta

