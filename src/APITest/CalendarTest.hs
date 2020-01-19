{-# LANGUAGE OverloadedStrings #-}

module APITest.CalendarTest
    ( runCalendarTest
    ) where

import Cria

import Record.Calendar

runCalendarTest :: CriaClient -> IO ()
runCalendarTest cli = do
  putStrLn "About to test getCalendarTest"
  putStrLn "Using week of the end of the Mayan 6th world ...."
  res <- signAndRun cli $ getCalendar (Just "2012-12-17") (Just "2012-12-21")
  case res of
    Left err -> putStrLn $ "Couldn't get clock..." ++ show err
    Right nextCalendarList -> do
      putStrLn "Success In Calendar:"
      print nextCalendarList
