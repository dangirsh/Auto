{-# LANGUAGE OverloadedStrings #-}

module Message (
      Message (..)
) where

import Data.Aeson (FromJSON, parseJSON, (.:), (.:?), Value(Object))
import Command
import Telemetry


data Message = CmdMessage Command | TlmMessage Telemetry deriving Show


instance FromJSON Message where
    parseJSON (Object o) = do
        mid <- o .: "mid"
        cc <- o .:? "cc"
        parameters <- o .: "parameters"
        return $ case cc of
            Nothing -> TlmMessage $ Telemetry (read mid) parameters
            Just i  -> CmdMessage $ Command (read mid) i parameters
    parseJSON _ = undefined