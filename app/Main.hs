module Main where

import Lib

import System.Environment

main :: IO ()
main = do
  key <- getEnv "ALPACA_KEY"
  secret <- getEnv "ALPACA_SECRET"
  runAPITest key secret
