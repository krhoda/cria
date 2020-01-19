{-# LANGUAGE OverloadedStrings #-}

module APITest.ClockTest
    ( runClockTest
    ) where

import Cria

import Record.Clock

runClockTest :: CriaClient -> IO ()
runClockTest cli = do
  putStrLn "About to test getClock"
  res <- signAndRun cli getClock
  case res of
    Left err -> putStrLn $ "Couldn't get clock..." ++ show err
    Right nextClock -> do
      putStrLn "Success In Clock:"
      print nextClock
