{-# LANGUAGE DeriveGeneric #-}

module Telemetry (
    Telemetry(Telemetry)
) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import Types
import Parameter()

instance FromJSON Telemetry


instance Show Telemetry where

    show (Telemetry ps) = "TLM: " ++ show ps
