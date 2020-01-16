{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( runAPITest
    ) where

import Alparseable
import Account as Acct
import Cria
import Watchlist as Wl

import Data.Text (Text, unpack)

testPrefix :: Int -> String -> IO Int
testPrefix x y = do
  print $ "Starting test #" ++ show x ++ " Desc: " ++ y
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
  (nextStep, watchlistID) <- testGetWatchlists cli step
  case watchlistID  of
    Nothing -> do
      putStrLn "No ID returned stopping Watchlist tests..."
      return nextStep
    Just wlID -> do
      lRes <- runReq cli (signReq cli getWatchList (unpack wlID))

      handleSimpleRes lRes (\x -> print x)

      -- POWT:
      -- followingGoogle <- isFollowingGoogle lRes
      -- finalStep <- toggleFollowGoogle cli True
      -- finalStep <- toggleFollowGoogle cli True

      return nextStep

testGetWatchlists :: CriaClient -> Int -> IO (Int, Maybe Text)
testGetWatchlists cli step = do
  wRes <- signAndRun cli getWatchLists
  nextStep <- testPrefix step "Testing List all Watchlists"
  case wRes of
        Left err -> do
          putStrLn $ "Error: " ++ show err
          return (nextStep, Nothing)

        Right wl -> do
          print wl
          return (nextStep, Just (Wl.watchlist_id $ head wl))

-- toggleFollowGoogle :: CriaClient -> Bool -> Int -> IO (Int)

handleSimpleRes :: (Show a) => Either a b -> (b -> IO ()) -> IO ()
handleSimpleRes x f = do
  case x of
    Left y -> putStrLn $ "Error: " ++ show y
    Right z ->  f z

handleTestRes :: (Show a) => Either a b -> (a -> IO c) -> (b -> IO c) -> IO c
handleTestRes x f g = do
  case x of
    Left y -> f y
    Right z ->  g z

useLive = False

firstStep :: Int
firstStep = 1

runAPITest :: String -> String -> IO ()
runAPITest key secret = do
  cli <- return (configCria (key, secret, useLive))
  accountStep <- testGetAccount cli firstStep
  watchListStep <- testWatchlistRoutes cli accountStep
  print "DONE!"
