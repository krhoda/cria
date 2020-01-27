{-# LANGUAGE DeriveGeneric #-}

module Record.Position where

import Data.Aeson
import Data.Text (Text)

import GHC.Generics (Generic)

data Position = Position {
 asset_id :: Text
 ,symbol :: Text
 ,exchange :: Text
 ,asset_class :: Text
 ,avg_entry_price :: Text
 ,qty :: Text
 ,side :: Text
 ,market_value :: Text
 ,cost_basis :: Text
 ,unrealized_pl :: Text
 ,unrealized_plpc :: Text
 ,unrealized_intraday_pl :: Text
 ,unrealized_intraday_plpc :: Text
 ,current_price :: Text
 ,lastday_price :: Text
 ,change_today :: Text
 } deriving (Show, Eq, Generic)

instance FromJSON Position
