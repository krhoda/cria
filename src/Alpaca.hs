{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Alpaca where

import Data.Aeson
import Data.Text (Text)

import GHC.Generics (Generic)

import Servant.API

import Record.Account
import Record.AccountConfiguration

import Record.Watchlist
import Record.Req.WatchlistPost
import Record.Req.WatchlistSymbolPost

type AlpacaAccount =
  -- View Account by Key/Secret
  -- getAccount
  "account"
  :> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> Get '[JSON] Account

  -- Get current account configuration
  -- getAccountConfig
  :<|> "account"
  :> "configurations"
  :> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> Get '[JSON] AccountConfiguration

  -- Update account configuration
  -- updateAccountConfig
  :<|> "account"
  :> "configurations"
  :> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> ReqBody '[JSON] AccountConfiguration
  :> Patch '[JSON] AccountConfiguration

type AlpacaWatchlist =
  -- List watchlists.
  -- getWatchlists
  "watchlists"
  :> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> Get '[JSON] [Watchlist]

  -- Get a particular watchlist.
  -- getWatchlist
  :<|> "watchlists"
  :> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> Capture "id" String
  :> Get '[JSON] Watchlist

  -- Create a Watchlist, returns list of all watchlists.
  -- createWatchlist
  :<|> "watchlists"
  :> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> ReqBody '[JSON] WatchlistPost
  :> Post '[JSON] Watchlist

  -- Update a particular watchlist
  -- updateWatchlist
  :<|> "watchlists"
  :> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> Capture "id" String
  :> ReqBody '[JSON] WatchlistPost
  :> Put '[JSON] Watchlist

  -- Add a symbol to watchlist
  -- addSymbolWatchlist
  :<|> "watchlists"
  :> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> Capture "id" String
  :> ReqBody '[JSON] WatchlistSymbolPost
  :> Post '[JSON] Watchlist

  -- Delete Symbol from Watchlist
  -- deleteSymbolWatchlist
  :<|> "watchlists"
  :> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> Capture "id" String
  :> Capture "symbol" String
  :> Delete '[JSON] Watchlist

  -- Delete Watchlist. TODO: WHAT DOES IT RETURN?
  -- deleteWatchlist
  :<|> "watchlists"
  :> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> Capture "id" String
  :> DeleteNoContent '[JSON] NoContent
