{-# LANGUAGE DeriveGeneric #-}

module Command (
     Command (Command)
) where

import Numeric (showHex)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import Parameter
import Common


type CommandCode = Byte


data Command = Command {
    cc :: CommandCode
   ,parameters :: [Parameter]
} deriving (Generic)

instance FromJSON Command


instance Show Command where

    show (Command c ps) = "CMD: " ++ " cc:" ++ showHex c " " ++ show ps

