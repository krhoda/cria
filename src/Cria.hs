module Cria where

import Data.Text (Text, unpack)
import Data.Proxy

import Alpaca

import Servant.Client

cira :: Proxy Alpaca
cira = Proxy

account = client cira
paperAlpacaBase = BaseUrl Https "paper-api.alpaca.markets" 443 "/v2"
