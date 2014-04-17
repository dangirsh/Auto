module Telemetry (
    Telemetry (..)
) where

import Data.BitVector (fromBool)
import Common
import CCSDS
import Numeric (showHex)
import Parameter


data Telemetry = Telemetry MessageID [Parameter]


instance Show Telemetry where

    show (Telemetry mid ps) = "TLM: mid:"
                             ++ showHex mid " "
                             ++ show ps


instance CCSDS Telemetry where

    packetType = const $ fromBool False

    applicationProcessId (Telemetry mid _) = safeBitVec 11 mid

    secondaryHeader =  const $ [0, 0, 0, 0, 0, 0] -- timestamp

    payload (Telemetry _ ps) = concatMap packParam ps
