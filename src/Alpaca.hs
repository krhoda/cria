{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Alpaca where

import Data.Aeson
import Data.Text (Text)

import GHC.Generics (Generic)

import Servant.API

import Account
import Watchlist

data WatchlistPost = WatchlistPost {
  name :: Text,
  symbols :: [Text]
  } deriving (Show, Eq, Generic)
instance FromJSON WatchlistPost
instance ToJSON WatchlistPost

newtype WatchlistSymbolPost = WatchlistSymbolPost {
  symbol :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON WatchlistSymbolPost
instance ToJSON WatchlistSymbolPost

type AlpacaAccount =
  -- View Account by Key/Secret
  Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> Get '[JSON] Account

type AlpacaWatchlist =
  -- List watchlists.
  Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> Get '[JSON] [Watchlist]

  -- Get a particular watchlist.
  :<|> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> Capture "id" String
  :> Get '[JSON] Watchlist

  -- Create a Watchlist, returns list of all watchlists.
  :<|> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> ReqBody '[JSON] [WatchlistPost]
  :> Post '[JSON] [Watchlist]

  -- Update a particular watchlist
  :<|> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> Capture "id" String
  :> ReqBody '[JSON] [WatchlistPost]
  :> Put '[JSON] Watchlist

  -- Add a symbol to watchlist
  :<|> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> Capture "id" String
  :> ReqBody '[JSON] [WatchlistSymbolPost]
  :> Post '[JSON] Watchlist

  -- Delete Symbol from Watchlist
  :<|> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> Capture "id" String
  :> Capture "symbol" String
  :> Delete '[JSON] Watchlist

  -- Delete Watchlist. TODO: WHAT DOES IT RETURN?
  -- :<|> "watchlists"
  -- :> Header "APCA-API-KEY-ID" String
  -- :> Header "APCA-API-SECRET-KEY" String
  -- :> Capture "id" String
  -- :> Delete '[JSON] Watchlist
