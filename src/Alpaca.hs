{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Alpaca where

import Data.Aeson
import Data.Text (Text)

import GHC.Generics (Generic)

import Servant.API

import Account
import Watchlist
import RequestBodies

type AlpacaAccount =
  -- View Account by Key/Secret
  "account"
  :> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> Get '[JSON] Account

type AlpacaWatchlist =
  -- List watchlists.
  "watchlists"
  :> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> Get '[JSON] [Watchlist]

  -- Get a particular watchlist.
  :<|> "watchlists"
  :> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> Capture "id" String
  :> Get '[JSON] Watchlist

  -- Create a Watchlist, returns list of all watchlists.
  :<|> "watchlists"
  :> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> ReqBody '[JSON] [WatchlistPost]
  :> Post '[JSON] [Watchlist]

  -- Update a particular watchlist
  :<|> "watchlists"
  :> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> Capture "id" String
  :> ReqBody '[JSON] WatchlistPost
  :> Put '[JSON] Watchlist

  -- Add a symbol to watchlist
  :<|> "watchlists"
  :> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> Capture "id" String
  :> ReqBody '[JSON] WatchlistSymbolPost
  :> Post '[JSON] Watchlist

  -- Delete Symbol from Watchlist
  :<|> "watchlists"
  :> Header "APCA-API-KEY-ID" String
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
