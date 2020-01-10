{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Alpaca where

import Servant.API

import Account
import Watchlist

type Alpaca = "account"
  :> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> Get '[JSON] Account
  :<|> "watchlists"
  :> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> Get '[JSON] [Watchlist]
  :<|> "watchlists"
  :> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> Capture "id" String
  :> Get '[JSON] Watchlist
