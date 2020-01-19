{-# LANGUAGE TypeOperators #-}

module Cria where

import Alpaca

import Data.Text (Text, unpack)
import Data.Proxy

import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import GHC.Generics (Generic)

import Servant.API
import Servant.Client

-- API Base Choices.
paperAlpacaBase = BaseUrl Https "paper-api.alpaca.markets" 443 "/v2"
trueAlpacaBase = BaseUrl Https "api.alpaca.markets" 443 "/v2"

type CriaError = ClientError

-- Proxy APIs exposed for custom usage. Consider not exposing?
accountProxy :: Proxy AlpacaAccount
accountProxy = Proxy

watchlistProxy :: Proxy AlpacaWatchlist
watchlistProxy = Proxy

-- Routes for public consumption.
accountRoutes = client accountProxy
watchlistRoutes = client watchlistProxy

-- Pre-pattern-matched Requests.
getAccount :<|>
  getAccountConfig :<|>
  updateAccountConfig = accountRoutes

getWatchlists :<|>
  getWatchlist :<|>
  createWatchlist :<|>
  updateWatchlist :<|>
  addSymbolWatchlist :<|>
  deleteSymbolWatchlist :<|>
  deleteWatchlist = watchlistRoutes

-- Reduces boilerplate by applying configuration to requests as needed.
data CriaClient = CriaClient {
  key :: String
  ,secret :: String
  ,live :: Bool
  } deriving (Show, Eq)

-- CriaClient Creation / Usage.
configCria :: (String, String, Bool) -> CriaClient
configCria (key, secret, live) = CriaClient {
                                    key = key,
                                    secret = secret,
                                    live = live
                                    }

-- Apply credentials to request -- always required.
signReq :: CriaClient -> (Maybe String -> Maybe String -> a) -> a
signReq x y = y (Just (key x)) (Just (secret x))

-- Run satisfied request.
runReq :: CriaClient -> ClientM a -> IO (Either ClientError a)
runReq cli req = do
  env <- criaEnv (live cli)
  res <- runClientM req env
  return res

-- Sign and Run is a helper for requests without any other params.
signAndRun :: CriaClient -> (Maybe String -> Maybe String -> ClientM a) -> IO (Either ClientError a)
signAndRun x y = runReq x (signReq x y)

-- Used to run requests with correct base.
criaEnv :: Bool -> IO ClientEnv
criaEnv x = do
  mgmt <-newManager tlsManagerSettings
  if x
      then return (mkClientEnv mgmt trueAlpacaBase)
      else return (mkClientEnv mgmt paperAlpacaBase)
