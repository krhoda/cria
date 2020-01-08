
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module OutPosition where

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
    topLevelAssetId :: Text,
    topLevelUnrealizedIntradayPlpc :: Text,
    topLevelExchange :: Text,
    topLevelUnrealizedPlpc :: Text,
    topLevelSymbol :: Text,
    topLevelChangeToday :: Text,
    topLevelAvgEntryPrice :: Text,
    topLevelAssetClass :: Text,
    topLevelLastdayPrice :: Text,
    topLevelMarketValue :: Text,
    topLevelUnrealizedPl :: Text,
    topLevelSide :: Text,
    topLevelUnrealizedIntradayPl :: Text,
    topLevelCurrentPrice :: Text,
    topLevelCostBasis :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) = TopLevel <$> v .:  "qty" <*> v .:  "asset_id" <*> v .:  "unrealized_intraday_plpc" <*> v .:  "exchange" <*> v .:  "unrealized_plpc" <*> v .:  "symbol" <*> v .:  "change_today" <*> v .:  "avg_entry_price" <*> v .:  "asset_class" <*> v .:  "lastday_price" <*> v .:  "market_value" <*> v .:  "unrealized_pl" <*> v .:  "side" <*> v .:  "unrealized_intraday_pl" <*> v .:  "current_price" <*> v .:  "cost_basis"
  parseJSON _          = mzero


instance ToJSON TopLevel where
  toJSON     (TopLevel {..}) = object ["qty" .= topLevelQty, "asset_id" .= topLevelAssetId, "unrealized_intraday_plpc" .= topLevelUnrealizedIntradayPlpc, "exchange" .= topLevelExchange, "unrealized_plpc" .= topLevelUnrealizedPlpc, "symbol" .= topLevelSymbol, "change_today" .= topLevelChangeToday, "avg_entry_price" .= topLevelAvgEntryPrice, "asset_class" .= topLevelAssetClass, "lastday_price" .= topLevelLastdayPrice, "market_value" .= topLevelMarketValue, "unrealized_pl" .= topLevelUnrealizedPl, "side" .= topLevelSide, "unrealized_intraday_pl" .= topLevelUnrealizedIntradayPl, "current_price" .= topLevelCurrentPrice, "cost_basis" .= topLevelCostBasis]
  toEncoding (TopLevel {..}) = pairs  ("qty" .= topLevelQty<>"asset_id" .= topLevelAssetId<>"unrealized_intraday_plpc" .= topLevelUnrealizedIntradayPlpc<>"exchange" .= topLevelExchange<>"unrealized_plpc" .= topLevelUnrealizedPlpc<>"symbol" .= topLevelSymbol<>"change_today" .= topLevelChangeToday<>"avg_entry_price" .= topLevelAvgEntryPrice<>"asset_class" .= topLevelAssetClass<>"lastday_price" .= topLevelLastdayPrice<>"market_value" .= topLevelMarketValue<>"unrealized_pl" .= topLevelUnrealizedPl<>"side" .= topLevelSide<>"unrealized_intraday_pl" .= topLevelUnrealizedIntradayPl<>"current_price" .= topLevelCurrentPrice<>"cost_basis" .= topLevelCostBasis)




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

