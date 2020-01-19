{-# LANGUAGE OverloadedStrings #-}

module IntegrationTest
    ( runAPITest
    ) where

-- import Record.Alparseable
import APITest.AccountTest
import APITest.WatchlistTest

import Cria


testPrefix :: Int -> String -> IO Int
testPrefix x y = do
  print $ "Starting test #" ++ show x ++ " Desc: " ++ y
  putStrLn ""
  return (x + 1)

useLive = False

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

  step2 <- testPrefix step1 "Test Watchlist Routes:"
  runWatchlistTest cli

  -- -- TODO: MORE WISHFUL THINKING

  -- TODO: Put this after trading test.
  step3 <- testPrefix step2 "Test Account Routes:"
  runAccountTest cli
