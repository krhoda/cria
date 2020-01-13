{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( runAPITest
    ) where

import Alparseable
import Account as Acct
import Cria
import Watchlist as Wl

import Data.Text (unpack)

testPrefix :: Int -> String -> IO Int
testPrefix x y = do
  print $ "Starting test #" ++ (show x) ++ " Desc: " ++ y
  putStrLn ""
  return (x + 1)

testGetAccount :: CriaClient -> Int -> IO Int
testGetAccount cli step = do
  nextStep <- testPrefix step "Testing Get Account"
  res <- signAndRun cli getAccount
  case res of
        Left err -> do
          putStrLn $ "Error: " ++ show err
          return nextStep

        Right acct -> do
          print acct
          print (alparse (Acct.status acct)) -- Will Print Nothing.
          print (alparse (buying_power acct))
          print (alparse (cash acct))
          return nextStep

testWatchlistRoutes :: CriaClient -> Int -> IO Int
testWatchlistRoutes cli step = do
  nextStep <- testPrefix step "Testing List all Watchlists"

  wRes <- signAndRun cli getWatchLists
  case wRes of
        Left err -> do
          putStrLn $ "Error: " ++ show err
          return nextStep

        Right wl -> do
          print wl

          finalStep <- testPrefix nextStep "Testing get Watchlist by ID"

          x <- return (Wl.watchlist_id $ head wl)

          lRes <- runReq cli (signReq cli getWatchList (unpack x))
          case lRes of
            Left err -> do
              putStrLn $ "Error: " ++ show err
              return finalStep

            Right wl' -> do
              print wl'
              return finalStep

useLive = False

firstStep :: Int
firstStep = 1

runAPITest :: String -> String -> IO ()
runAPITest key secret = do
  cli <- return (configCria (key, secret, useLive))
  accountStep <- testGetAccount cli firstStep
  watchListStep <- testWatchlistRoutes cli accountStep
  print "DONE!"
