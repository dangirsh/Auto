module Command (
     Command (..)
) where

import Data.BitVector hiding (showHex)
import Text.PrettyPrint.GenericPretty
import Common
import CCSDS
import Numeric (showHex)
import Parameter


data Command = Command MessageID CommandCode [Parameter]


instance Show Command where

    show (Command mid cc ps) = "CMD: mid:"
                             ++ showHex mid ""
                             ++ " cc:"
                             ++ showHex cc " "
                             ++ show ps


instance CCSDS Command where

    packetType = const $ fromBool True

    applicationProcessId (Command mid _ _) = safeBitVec 11 mid

    secondaryHeader (Command _ cc _) = [cc, 0]

    payload (Command _ _ ps) = concatMap packParam ps
