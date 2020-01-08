module Cria where

import Alpaca
import Data.Text (Text, unpack)
import Data.Proxy

import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Client

paperAlpacaBase = BaseUrl Https "paper-api.alpaca.markets" 443 "/v2"
trueAlpacaBase = BaseUrl Https "api.alpaca.markets" 443 "/v2"

data CriaClient = CriaClient {
  proxy :: Proxy Alpaca,
  key :: String,
  secret :: String,
  live :: Bool
  } deriving (Show, Eq)

configCria :: (String, String, Bool) -> CriaClient
configCria (key, secret, live) = CriaClient {
  proxy = Proxy,
  key = key,
  secret = secret,
  live = live
  }

signReq :: CriaClient -> (Maybe String -> Maybe String -> a) -> a
signReq x y = y (Just (key x)) (Just (secret x))

runReq :: CriaClient -> ClientM a -> IO (Either ClientError a)
runReq x y = do
  env <- criaEnv (live x)
  res <- runClientM y env
  return res

signAndRun :: CriaClient -> (Maybe String -> Maybe String -> ClientM a) -> IO (Either ClientError a)
signAndRun x y = runReq x (signReq x y)

routes :: CriaClient -> Client ClientM Alpaca
routes x = client (proxy x)

-- Get Routes:
account = client apiProxy

apiProxy :: Proxy Alpaca
apiProxy = Proxy

criaEnv :: Bool -> IO ClientEnv
criaEnv x = do
  mgmt <-newManager tlsManagerSettings
  if x
      then return (mkClientEnv mgmt trueAlpacaBase)
      else return (mkClientEnv mgmt paperAlpacaBase)
