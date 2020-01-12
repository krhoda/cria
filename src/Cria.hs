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

data CriaClient = CriaClient {
  key :: String,
  secret :: String,
  live :: Bool
  } deriving (Show, Eq)

accountProxy :: Proxy AlpacaAccount
accountProxy = Proxy

watchlistProxy :: Proxy AlpacaWatchlist
watchlistProxy = Proxy

accountRoutes = ("account", client accountProxy)
watchlistRoutes = ("watchlists", client watchlistProxy)

configCria :: (String, String, Bool) -> CriaClient
configCria (key, secret, live) = CriaClient {
                                    key = key,
                                    secret = secret,
                                    live = live
                                    }

signReq :: CriaClient -> (Maybe String -> Maybe String -> a) -> a
signReq x y = y (Just (key x)) (Just (secret x))

runReq :: CriaClient -> String -> ClientM a -> IO (Either ClientError a)
runReq cli slug req = do
  env <- criaEnv slug (live cli)
  res <- runClientM req env
  return res

signAndRun :: CriaClient -> String -> (Maybe String -> Maybe String -> ClientM a) -> IO (Either ClientError a)
signAndRun x y z = runReq x y (signReq x z)

criaEnv :: String -> Bool -> IO ClientEnv
criaEnv x y = do
  mgmt <-newManager tlsManagerSettings
  if y
      then return (mkClientEnv mgmt (trueAlpacaBase x))
      else return (mkClientEnv mgmt (paperAlpacaBase x))
