{-# LANGUAGE OverloadedStrings #-}

module Parse where

import Control.Monad
import Control.Applicative
import Data.Aeson (FromJSON, parseJSON, (.:), (.:?), Value(Object), eitherDecode)
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Search (replace)
import qualified Data.ByteString.Lazy.Char8 as C
import Controller
import Message
import Command
import Telemetry


parseFile :: (FromJSON a) => FilePath -> IO a
parseFile file = do
    -- parse <$> cleanFile file
    parse <$> C.filter (/= '\n') <$> B.readFile file


parse :: (FromJSON a) => B.ByteString -> a
parse b = case eitherDecode b of
            Right x -> x
            Left  e -> error e

--cleanFile :: FilePath -> IO ByteString
--cleanFile file =
--    ls <- lines <$> readFile file
--    return . B.pack . removeNewlines . removeComments $ ls
