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

firstStep :: Int
firstStep = 1

runAPITest :: String -> String -> IO ()
runAPITest key secret = do
  runIntegrationTest key secret
  -- cli <- return (configCria (key, secret, useLive))
  -- accountStep <- testGetAccount cli firstStep
  -- watchlistStep <- testWatchlistRoutes cli accountStep
  -- print "DONE!"

runIntegrationTest :: String -> String -> IO ()
runIntegrationTest key secret = do
  cli <- return (configCria (key, secret, useLive))
  putStrLn "About to start integration tests:"

  step2 <- testPrefix firstStep "Test Account Routes:"
  runAccountTest  cli

  step3 <- testPrefix step2 "Test Watchlist Routes:"
  runWatchlistTest cli
  -- -- TODO: MORE WISHFUL THINKING
