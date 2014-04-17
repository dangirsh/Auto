module Telemetry (
    Telemetry (..)
) where

import Data.BitVector (fromBool)
import Common
import CCSDS
import Parameter


data Telemetry = Telemetry MessageID [Parameter] deriving (Show)


instance CCSDS Telemetry where

    packetType = const $ fromBool False

    applicationProcessId (Telemetry mid _) = safeBitVec 11 mid

    secondaryHeader =  const $ [0, 0, 0, 0, 0, 0] -- timestamp

    payload (Telemetry _ ps) = concatMap packParam ps
