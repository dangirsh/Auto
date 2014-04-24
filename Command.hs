{-# LANGUAGE DeriveGeneric #-}

module Command (
     Command (Command)
) where

import Control.Applicative ((<$>))
import Numeric (showHex)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import Auto
import Types
import Parameter()


instance FromJSON Command


instance AutoShow Command where

    autoShow (Command c ps) = do
        sp <- concat <$> mapM autoShow ps
        return $ "CMD::" ++ " cc:" ++ showHex c " " ++ sp