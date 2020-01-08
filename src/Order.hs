
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module OutOrder where

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
    topLevelStatus :: Text,
    topLevelQty :: Text,
    topLevelLimitPrice :: Text,
    topLevelStopPrice :: Text,
    topLevelAssetId :: Text,
    topLevelExpiredAt :: Text,
    topLevelFilledQty :: Text,
    topLevelExtendedHours :: Bool,
    topLevelSymbol :: Text,
    topLevelAssetClass :: Text,
    topLevelFilledAt :: Text,
    topLevelTimeInForce :: Text,
    topLevelUpdatedAt :: Text,
    topLevelClientOrderId :: Text,
    topLevelCreatedAt :: Text,
    topLevelId :: Text,
    topLevelCanceledAt :: Text,
    topLevelType :: Text,
    topLevelSide :: Text,
    topLevelSubmittedAt :: Text,
    topLevelFilledAvgPrice :: Text,
    topLevelFailedAt :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) = TopLevel <$> v .:  "status" <*> v .:  "qty" <*> v .:  "limit_price" <*> v .:  "stop_price" <*> v .:  "asset_id" <*> v .:  "expired_at" <*> v .:  "filled_qty" <*> v .:  "extended_hours" <*> v .:  "symbol" <*> v .:  "asset_class" <*> v .:  "filled_at" <*> v .:  "time_in_force" <*> v .:  "updated_at" <*> v .:  "client_order_id" <*> v .:  "created_at" <*> v .:  "id" <*> v .:  "canceled_at" <*> v .:  "type" <*> v .:  "side" <*> v .:  "submitted_at" <*> v .:  "filled_avg_price" <*> v .:  "failed_at"
  parseJSON _          = mzero


instance ToJSON TopLevel where
  toJSON     (TopLevel {..}) = object ["status" .= topLevelStatus, "qty" .= topLevelQty, "limit_price" .= topLevelLimitPrice, "stop_price" .= topLevelStopPrice, "asset_id" .= topLevelAssetId, "expired_at" .= topLevelExpiredAt, "filled_qty" .= topLevelFilledQty, "extended_hours" .= topLevelExtendedHours, "symbol" .= topLevelSymbol, "asset_class" .= topLevelAssetClass, "filled_at" .= topLevelFilledAt, "time_in_force" .= topLevelTimeInForce, "updated_at" .= topLevelUpdatedAt, "client_order_id" .= topLevelClientOrderId, "created_at" .= topLevelCreatedAt, "id" .= topLevelId, "canceled_at" .= topLevelCanceledAt, "type" .= topLevelType, "side" .= topLevelSide, "submitted_at" .= topLevelSubmittedAt, "filled_avg_price" .= topLevelFilledAvgPrice, "failed_at" .= topLevelFailedAt]
  toEncoding (TopLevel {..}) = pairs  ("status" .= topLevelStatus<>"qty" .= topLevelQty<>"limit_price" .= topLevelLimitPrice<>"stop_price" .= topLevelStopPrice<>"asset_id" .= topLevelAssetId<>"expired_at" .= topLevelExpiredAt<>"filled_qty" .= topLevelFilledQty<>"extended_hours" .= topLevelExtendedHours<>"symbol" .= topLevelSymbol<>"asset_class" .= topLevelAssetClass<>"filled_at" .= topLevelFilledAt<>"time_in_force" .= topLevelTimeInForce<>"updated_at" .= topLevelUpdatedAt<>"client_order_id" .= topLevelClientOrderId<>"created_at" .= topLevelCreatedAt<>"id" .= topLevelId<>"canceled_at" .= topLevelCanceledAt<>"type" .= topLevelType<>"side" .= topLevelSide<>"submitted_at" .= topLevelSubmittedAt<>"filled_avg_price" .= topLevelFilledAvgPrice<>"failed_at" .= topLevelFailedAt)




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

