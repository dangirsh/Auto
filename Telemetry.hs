{-# LANGUAGE DeriveGeneric #-}

module Telemetry (
    Telemetry (..)
) where

import Common
import CCSDS
import Numeric (showHex)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import Parameter


data Telemetry = Telemetry {parameters :: [Parameter]} deriving (Generic)

instance FromJSON Telemetry


instance Show Telemetry where

    show (Telemetry ps) = "TLM: " ++ show ps
