{-# LANGUAGE OverloadedStrings, DeriveGeneric, FlexibleInstances, RankNTypes #-}

module Message (
    Message (..)
   ,MessageDef (..)
) where

import Data.Aeson (FromJSON, parseJSON, (.:), Value(Object))
import GHC.Generics (Generic)
import Control.Applicative ((<$>), (<*>))
import Data.BitVector (fromBool)
import Common
import CCSDS
import Command
import Telemetry
import Parameter
import Variable
import Types




instance (FromJSON a) => FromJSON (Message a) where

    parseJSON json@(Object o) = Message <$> (read <$> o .: "mid") <*> parseJSON json
    parseJSON _ = error "Invalid message definition."


instance (FromJSON a) => FromJSON (MessageDef a)


instance CCSDS (Message Telemetry) where

    packetType (Message _ (Telemetry _)) = fromBool False

    applicationProcessId (Message mid _) = safeBitVec 11 mid

    secondaryHeader (Message _ (Telemetry _)) = [0, 0, 0, 0, 0, 0] -- timestamp

    payload (Message _ (Telemetry ps)) = concat <$> mapM packParam ps



instance CCSDS (Message Command) where

    packetType (Message _ (Command _ _)) = fromBool True

    applicationProcessId (Message mid _) = safeBitVec 11 mid

    secondaryHeader (Message _ (Command cc _)) = [cc, 0]

    payload (Message _ (Command _ ps)) = concat <$> mapM packParam ps