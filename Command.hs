module Command (
     Command (Command)
) where

import Control.Applicative ((<$>))
import Common
import Data.Aeson (FromJSON)
import Auto
import Types
import Parameter()


instance FromJSON Command


instance AutoShow Command where

    autoShow (Command c ps) = do
        sp <- concatMap ("\n\t" ++) <$> mapM autoShow ps
        return $ "cc:" ++ showHex' c ++ " " ++ sp