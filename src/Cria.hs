module Cria where

import Alpaca
import Data.Text (Text, unpack)
import Data.Proxy

import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client

paperAlpacaBase :: String -> BaseUrl
paperAlpacaBase x = BaseUrl Https "paper-api.alpaca.markets" 443 ("/v2/" ++ x)

trueAlpacaBase :: String -> BaseUrl
trueAlpacaBase x = BaseUrl Https "api.alpaca.markets" 443 ("/v2/" ++ x)

data AlpacaApi = AlpacaAccount | AlpacaWatchlist
  deriving (Show, Eq)

slug :: AlpacaApi -> String
slug AlpacaAccount = "account"
slug AlpacaWatchlist = "wishlist"

-- routes ::
routes :: CriaClient -> (CriaClient -> Proxy AlpacaApi) -> Client ClientM AlpacaApi
routes x y = client (y x)


-- routes :: AlpacaWatchlist -> Client ClientM a
-- routes AlpacaWatchlist = client Proxy AlpacaWatchlist

data CriaClient = CriaClient {
  account :: Proxy AlpacaAccount,
  watchlist :: Proxy AlpacaWatchlist,
  key :: String,
  secret :: String,
  live :: Bool
  } deriving (Show, Eq)

configCria :: (String, String, Bool) -> CriaClient
configCria (key, secret, live) = CriaClient {
                                    account = Proxy,
                                    watchlist = Proxy,
                                    key = key,
                                    secret = secret,
                                    live = live
                                    }

signReq :: CriaClient -> (Maybe String -> Maybe String -> a) -> a
signReq x y = y (Just (key x)) (Just (secret x))

runReq :: CriaClient -> Proxy AlpacaApi -> ClientM a -> IO (Either ClientError a)
runReq cli route req = do
  env <- criaEnv (fmap slug route) (live cli)
  res <- runClientM req env
  return res

signAndRun :: CriaClient -> Proxy AlpacaApi -> (Maybe String -> Maybe String -> ClientM a) -> IO (Either ClientError a)
signAndRun x y z = runReq x y (signReq x z)

criaEnv :: Proxy String -> Bool -> IO ClientEnv
criaEnv x y = do
  mgmt <-newManager tlsManagerSettings
  if y
      then return (mkClientEnv mgmt (trueAlpacaBase x))
      else return (mkClientEnv mgmt (paperAlpacaBase x))
