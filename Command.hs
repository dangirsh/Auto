module Command (
     Command (..)
) where

import Data.BitVector
import Text.PrettyPrint.GenericPretty
import Common
import CCSDS
import Parameter


data Command = Command MessageID CommandCode [Parameter] deriving (Show)

instance CCSDS Command where

    packetType = const $ fromBool True

    applicationProcessId (Command mid _ _) = safeBitVec 11 mid

    secondaryHeader (Command _ cc _) = [cc, 0]

    payload (Command _ _ ps) = concatMap packParam ps
