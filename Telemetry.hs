{-# LANGUAGE DeriveGeneric #-}

module Telemetry (
    Telemetry(Telemetry)
) where

import Control.Applicative ((<$>))
import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import Types
import Auto
import Parameter()

instance FromJSON Telemetry


instance AutoShow Telemetry where

    autoShow (Telemetry ps) = do
        sp <- concat <$> mapM autoShow ps
        return $ "TLM: " ++ sp
