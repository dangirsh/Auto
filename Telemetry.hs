module Telemetry (
    Telemetry(Telemetry)
) where

import Control.Applicative ((<$>))
import Data.Aeson (FromJSON)
import Types
import Auto
import Parameter()

instance FromJSON Telemetry


instance AutoShow Telemetry where

    autoShow (Telemetry ps) = do
        sp <- concatMap ("\n\t" ++) <$> mapM autoShow ps
        return sp
