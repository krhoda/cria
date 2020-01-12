{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( someFunc
    ) where

import Alparseable

import Account as Acct
import Cria as Cria

import Watchlist as Wl

import Data.Text (unpack)

import Servant.API

import System.Environment
import Servant.Client

someFunc :: IO ()
someFunc = do
  key <- getEnv "ALPACA_KEY"
  secret <- getEnv "ALPACA_SECRET"
  cli <- return (Cria.configCria (key, secret, False))

  -- account :<|>
  --   getWatchLists :<|>
  --   getWatchList :<|>
  --   updateWatchList :<|>
  --   addSymbolWatchList :<|>
  --   deleteSymbolWatchList <- return (Cria.routes cli)

  accountRoute <- return (account cli)
  accountReq <- return (routes cli)
  res <- signAndRun cli accountRoute accountReq
  case res of
        Left err -> putStrLn $ "Error: " ++ show err
        Right acct -> do
          print (alparse (Acct.status acct)) -- Will Print Nothing.
          print (alparse (buying_power acct))
          print (alparse (cash acct))


  -- wRes <- signAndRun cli getWatchLists
  -- case wRes of
  --       Left err -> putStrLn $ "Error: " ++ show err
  --       Right wl -> do
  --         print wl
  --         x <- return (Wl.watchlist_id $ head wl)
  --         lRes <- runReq cli (signReq cli getWatchList (unpack x))
  --         case lRes of
  --           Left err -> putStrLn $ "Error: " ++ show err
  --           Right wl' -> print wl'

          -- case x of
          --   Nothing -> putStrLn "ID was nothing"
          --   Just(x') -> do
