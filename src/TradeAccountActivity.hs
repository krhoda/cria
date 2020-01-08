
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module OutTradeAccountActivity where

import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (stderr, hPutStrLn)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           System.Environment (getArgs)
import           Control.Monad      (forM_, mzero, join)
import           Control.Applicative
import           Data.Aeson.AutoType.Alternative
import           Data.Aeson(eitherDecode, Value(..), FromJSON(..), ToJSON(..),
                            pairs,
                            (.:), (.:?), (.=), object)
import           Data.Monoid((<>))
import           Data.Text (Text)
import qualified GHC.Generics

data TopLevel = TopLevel { 
    topLevelQty :: Text,
    topLevelActivityType :: Text,
    topLevelSymbol :: Text,
    topLevelId :: Text,
    topLevelPrice :: Text,
    topLevelCumQty :: Text,
    topLevelType :: Text,
    topLevelSide :: Text,
    topLevelLeavesQty :: Text,
    topLevelTransactionTime :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) = TopLevel <$> v .:  "qty" <*> v .:  "activity_type" <*> v .:  "symbol" <*> v .:  "id" <*> v .:  "price" <*> v .:  "cum_qty" <*> v .:  "type" <*> v .:  "side" <*> v .:  "leaves_qty" <*> v .:  "transaction_time"
  parseJSON _          = mzero


instance ToJSON TopLevel where
  toJSON     (TopLevel {..}) = object ["qty" .= topLevelQty, "activity_type" .= topLevelActivityType, "symbol" .= topLevelSymbol, "id" .= topLevelId, "price" .= topLevelPrice, "cum_qty" .= topLevelCumQty, "type" .= topLevelType, "side" .= topLevelSide, "leaves_qty" .= topLevelLeavesQty, "transaction_time" .= topLevelTransactionTime]
  toEncoding (TopLevel {..}) = pairs  ("qty" .= topLevelQty<>"activity_type" .= topLevelActivityType<>"symbol" .= topLevelSymbol<>"id" .= topLevelId<>"price" .= topLevelPrice<>"cum_qty" .= topLevelCumQty<>"type" .= topLevelType<>"side" .= topLevelSide<>"leaves_qty" .= topLevelLeavesQty<>"transaction_time" .= topLevelTransactionTime)




parse :: FilePath -> IO TopLevel
parse filename = do
    input <- BSL.readFile filename
    case eitherDecode input of
      Left  err -> fatal $ case (eitherDecode input :: Either String Value) of
                           Left  err -> "Invalid JSON file: " ++ filename ++ " ++ err"
                           Right _   -> "Mismatched JSON value from file: " ++ filename
                                     ++ "\n" ++ err
      Right r   -> return (r :: TopLevel)
  where
    fatal :: String -> IO a
    fatal msg = do hPutStrLn stderr msg
                   exitFailure

main :: IO ()
main = do
  filenames <- getArgs
  forM_ filenames (\f -> parse f >>= (\p -> p `seq` putStrLn $ "Successfully parsed " ++ f))
  exitSuccess

