module Command (
     Command (..)
) where

import Data.BitVector
import Common
import CCSDS
import Parameter


data Command = Command MessageID CommandCode [Parameter]


instance CCSDS Command where

    packetType = const $ fromBool True

    applicationProcessId (Command mid _ _) = safeBitVec 11 mid

    secondaryHeader (Command _ cc _) = [cc, 0]

    payload (Command _ _ ps) = concatMap packParam ps


instance Show Command where

    show = showCCSDS