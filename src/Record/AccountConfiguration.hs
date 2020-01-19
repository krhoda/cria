{-# LANGUAGE DeriveGeneric #-}

module Record.AccountConfiguration where

import Data.Aeson
import Data.Text (Text)

import GHC.Generics (Generic)

data AccountConfiguration = AccountConfiguration {
    dtbp_check :: Text, -- "both" || "entry" || "exit"
    no_shorting :: Bool,
    suspend_trade :: Bool,
    trade_confirm_email :: Text -- "all" || "none"
  } deriving (Show, Eq, Generic)

instance FromJSON AccountConfiguration
instance ToJSON AccountConfiguration
