{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Alpaca where

import Data.Aeson
import Data.Text (Text)

import GHC.Generics (Generic)

import Servant.API

import Record.Account
import Record.AccountConfiguration

import Record.Clock
import Record.Calendar

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
  :> ReqBody '[JSON] AccountConfiguration
  :> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> Patch '[JSON] AccountConfiguration

type AlpacaCalendar =
  -- TODO: Put some guards arount inputs to start / end?

  -- Gets the calendar.
  -- getCalendar
  -- %Y-%m-%d
  "calendar"
  :> QueryParam "start" String
  :> QueryParam "end" String
  :> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> Get '[JSON] [Calendar]

type AlpacaClock =
  -- Gets the clock.
  -- getClock
  "clock"
  :> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> Get '[JSON] Clock

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
  :> Capture "id" String
  :> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> Get '[JSON] Watchlist

  -- Create a Watchlist, returns list of all watchlists.
  -- createWatchlist
  :<|> "watchlists"
  :> ReqBody '[JSON] WatchlistPost
  :> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> Post '[JSON] Watchlist

  -- Update a particular watchlist
  -- updateWatchlist
  :<|> "watchlists"
  :> Capture "id" String
  :> ReqBody '[JSON] WatchlistPost
  :> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> Put '[JSON] Watchlist

  -- Add a symbol to watchlist
  -- addSymbolWatchlist
  :<|> "watchlists"
  :> Capture "id" String
  :> ReqBody '[JSON] WatchlistSymbolPost
  :> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> Post '[JSON] Watchlist

  -- Delete Symbol from Watchlist
  -- deleteSymbolWatchlist
  :<|> "watchlists"
  :> Capture "id" String
  :> Capture "symbol" String
  :> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> Delete '[JSON] Watchlist

  -- Delete Watchlist. TODO: WHAT DOES IT RETURN?
  -- deleteWatchlist
  :<|> "watchlists"
  :> Capture "id" String
  :> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> DeleteNoContent '[JSON] NoContent
