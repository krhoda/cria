{-# LANGUAGE OverloadedStrings #-}

module IntegrationTest
    ( runAPITest
    ) where

import Record.Alparseable
import Record.Account as Acct
import Record.Asset as Ast
import Cria
import Record.RequestBodies as Req
import Record.Watchlist as Wl

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
  case watchlistID of
    Nothing -> do
      putStrLn "No ID returned stopping Watchlist tests..."
        -- TODO: PUT CREATE HERE:
      return nextStep
    Just wlID -> do
      lRes <- runReq cli (signReq cli getWatchlist (unpack wlID))
      anotherStep <- testPrefix step "Testing Get Watchlist... About to toggle google"
      testToggleWatchlistGoogle cli lRes
      finalStep <-  testPrefix step "Toggled Google"

      return finalStep

testToggleWatchlistGoogle :: CriaClient -> Either CriaError Watchlist -> IO ()
testToggleWatchlistGoogle _ (Left x) = print $ "Error: " ++ show x
testToggleWatchlistGoogle cli (Right wl) = do
  toggleWatchingGoogle cli (unpack (watchlist_id wl)) (isWatchingGoogle (assets wl))

        -- finalStep <- toggleWatchingGoogle cli nextStep (isWatchingGoogle asts)
        -- return finalStep

isWatchingGoogle :: Maybe [Asset] -> Bool
isWatchingGoogle Nothing = False
isWatchingGoogle (Just []) = False
isWatchingGoogle (Just (x : xs)) = case Ast.symbol x of
  "GOOG"  -> True
  _ -> isWatchingGoogle (Just xs)

googleReq :: WatchlistSymbolPost
googleReq = WatchlistSymbolPost "GOOG"

testWL :: WatchlistPost
testWL = WatchlistPost "flowers" ["FLWS"]

toggleWatchingGoogle :: CriaClient -> String -> Bool -> IO ()
toggleWatchingGoogle cli wlID True = do
  res <- runReq cli $ signReq cli deleteSymbolWatchlist wlID "GOOG"
  handleSimpleRes res print

toggleWatchingGoogle cli wlID False = do
  res <- runReq cli $ signReq cli addSymbolWatchlist wlID googleReq
  handleSimpleRes res print

testGetWatchlists :: CriaClient -> Int -> IO (Int, Maybe Text)
testGetWatchlists cli step = do
  wRes <- signAndRun cli getWatchlists
  nextStep <- testPrefix step "Testing List all Watchlists"
  case wRes of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      return (nextStep, Nothing)

    Right wl -> do
      print wl
      return (nextStep, Just (Wl.watchlist_id $ head wl))

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
  watchlistStep <- testWatchlistRoutes cli accountStep
  print "DONE!"
