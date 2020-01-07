{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Alpaca where

import Servant.API

import Account

type Alpaca = "account"
  :> Header "APCA-API-KEY-ID" String
  :> Header "APCA-API-SECRET-KEY" String
  :> Get '[JSON] Account
