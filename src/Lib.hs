{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Account
import Cria
import Alparseable

import System.Environment
import Servant.Client

someFunc :: IO ()
someFunc = do
  key <- getEnv "ALPACA_KEY"
  secret <- getEnv "ALPACA_SECRET"
  cli <- return (configCria (key, secret, False))

  -- account <- return (routes cli)

  res <- signAndRun cli account
  case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right acct -> do
          print (alparse (status acct)) -- Will Print Nothing.
          print (alparse (buying_power acct))
          print (alparse (cash acct))
