{-# LANGUAGE DeriveGeneric #-}


module Types where

import qualified Data.Map as M
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics


type Byte = Word8

type Frequency = Double

type MessageID = Byte

type CommandCode = Byte

--data Endianness = BE | LE


data Data = S   (String, Int)
          | B   Bool
          | I8  Int8
          | W8  Word8
          | I16 Int16
          | W16 Word16
          | I32 Int32
          | W32 Word32
          | I64 Int64
          | W64 Word64
          | F   Float
          | D   Double
          | Arr [Data]
          | Var String deriving (Show)


data Parameter = Parameter String Data


data Variable = Variable String [Data] deriving (Show)


data Command = Command {
    cc :: CommandCode
   ,arguments :: [Parameter]
} deriving (Generic)


data Telemetry = Telemetry {
    parameters :: [Parameter]
} deriving (Generic)


data Message a = Message MessageID a


data MessageDef a = MessageDef {
    variables :: [Variable]
   ,message :: Message a
} deriving (Generic)


data MessageMeta = MessageMeta {
     file :: FilePath
    ,frequency :: Double
    ,times :: Int
} deriving (Generic)


data Controller = Controller {
     meta :: ControllerMeta
    ,sequenced :: [MessageMeta]
    ,parallel :: [MessageMeta]
} deriving (Generic)


data ControllerMeta = ControllerMeta {
     ip :: String
    ,port :: Integer
} deriving (Generic)


data Config = Config {
    envC :: M.Map String Parameter
}
