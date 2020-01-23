{-# LANGUAGE OverloadedStrings #-}

module IntegrationTest
    ( runAPITest
    ) where

-- import Record.Alparseable
import APITest.AccountTest
import APITest.AssetTest
import APITest.CalendarTest
import APITest.ClockTest
import APITest.WatchlistTest

import Cria


testPrefix :: Int -> String -> IO Int
testPrefix x y = do
  print $ "Starting test #" ++ show x ++ " Desc: " ++ y
  putStrLn ""
  return (x + 1)

useLive = False
verbose = False

step1 :: Int
step1 = 1

-- TODO: REMOVE
runAPITest :: String -> String -> IO ()
runAPITest key secret = do
  runIntegrationTest key secret

runIntegrationTest :: String -> String -> IO ()
runIntegrationTest key secret = do
  cli <- return (configCria (key, secret, useLive))
  putStrLn "About to start integration tests:"
  putStrLn "Let's start simple, Clock then Calendar..."

  step2 <- testPrefix step1 "Test Clock Route Route:"
  runClockTest cli

  step3 <- testPrefix step2 "Test Calendar Route Route:"
  runCalendarTest cli

  putStrLn "Now let's look at read-only... Watchlists:"

  step4 <- testPrefix step3 "Test Watchlist Routes:"
  runWatchlistTest cli

  -- -- TODO: MORE WISHFUL THINKING
  putStrLn "Read only Account Routes:"

  -- TODO: Put this after trading test.
  step5 <- testPrefix step4 "Test Account Routes:"
  runAccountTest cli

  putStrLn "Read only Asset Routes:"
  step6 <- testPrefix step5 "Test Asset Routes:"
  runAssetTest cli verbose
