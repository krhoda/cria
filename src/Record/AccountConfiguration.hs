{-# LANGUAGE DeriveGeneric #-}

module Record.AccountConfiguration where

import Data.Aeson
import Data.Text (Text)

import GHC.Generics (Generic)

data AccountConfiguration = AccountConfiguration {
    trade_confirm_email :: Text,
    dtbp_check :: Text,
    no_shorting :: Bool,
    suspend_trade :: Bool
  } deriving (Show, Eq, Generic)

instance FromJSON AccountConfiguration
