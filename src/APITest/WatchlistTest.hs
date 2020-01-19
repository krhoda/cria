{-# LANGUAGE OverloadedStrings #-}

module APITest.WatchlistTest
    ( runWatchlistTest
    ) where

import Cria

import Record.Asset as Ast
import Record.Watchlist as Wl

import Record.Req.WatchlistPost
import Record.Req.WatchlistSymbolPost

import Data.Text (Text, pack, unpack)

printWLError :: String -> IO ()
printWLError x = print (x ++ " Ending Watchlist Test...")

runWatchlistTest :: CriaClient -> IO ()
runWatchlistTest cli = do
  putStrLn "Checking Existing Watchlists\n"
  testGetWatchlists cli
  putStrLn "Making New Watchlist\n"
  eitherWL <- makeTestWatchlist cli
  maybeWL <- checkTestWatchlist eitherWL
  case maybeWL of
    Nothing -> putStrLn "checkTestWatchlist Failed."

    Just wl -> do
      success <- checkToggleRoutes cli wl

      if not success
        then putStrLn "checkToggleRoutes Failed."

        else do
          delRes <- runReq cli $ (signReq cli deleteWatchlist) (unpack (watchlist_id wl))

          case delRes of
            Left err -> printWLError $ "deleteWatchList Failed: " ++ show err

            Right (_) -> do
              putStrLn "Checking for deleted watchlists."
              wRes <- signAndRun cli getWatchlists

              case wRes of
                Left err -> printWLError $ "Error: " ++ show err

                Right wlList -> do
                  shouldBeFalse <- return $ checkForWLID wlList (unpack (watchlist_id wl))
                  if shouldBeFalse
                    then printWLError "Still found watchlist after deletion."
                    else putStrLn "All Watchlist tests successful."

checkForWLID :: [Watchlist] -> String -> Bool
checkForWLID [] _ = False
checkForWLID (x : xs) y = if unpack (watchlist_id x) == y
                            then True
                            else checkForWLID xs y

makeTestWatchlist :: CriaClient -> IO (Either CriaError Watchlist)
makeTestWatchlist cli = runReq cli $ (signReq cli createWatchlist) testWL

checkTestWatchlist :: Either CriaError Watchlist -> IO (Maybe Watchlist)
checkTestWatchlist (Left err) = do
        printWLError $ "Cannot Create Watchlist! Error: " ++ (show err)
        return Nothing

checkTestWatchlist (Right wl) = do
  print "!!!"
  print wl
  print "!!!"
  putStrLn "Verify it does not have Google..."
  shouldBeFalse <- return $ checkForSym (assets wl) "GOOG"
  if shouldBeFalse
    then do
      printWLError "Found google in test watchlist. Should only find FLWS."
      return Nothing

    else do
      putStrLn "Verify it has flowers..."
      shouldBeTrue <- return $ checkForSym (assets wl) "FLWS"

      if not shouldBeTrue
        then do
          printWLError ("Did not find initial symbol, FLWS, in test watchlist: " ++ show (assets wl) ++ " ")
          return Nothing

        else return (Just wl)

checkToggleRoutes :: CriaClient -> Watchlist -> IO (Bool)
checkToggleRoutes cli wl = do
  addRes <- toggleWatchSym cli (unpack (watchlist_id wl)) "GOOG" True
  case addRes of
    Left err -> do
      printWLError $ show err
      return False

    Right wl2 -> do
      putStrLn "Toggled google!"
      shouldBeTrue <- return $ checkForSym (assets wl) "GOOG"

      if shouldBeTrue
        then do
          printWLError ("Did not find added symbol, GOOG, in test watchlist: " ++ show (assets wl) ++ " ")
          return shouldBeTrue

        else do
          putStrLn "Google Found! Now removing..."
          delRes <- toggleWatchSym cli (unpack (watchlist_id wl2)) "GOOG" False
          case delRes of
            Left err -> do
              printWLError $ show err
              return False
            Right wl3 -> do
                shouldBeFalse <- return $ checkForSym (assets wl3) "GOOG"
                if not shouldBeFalse
                  then do
                    printWLError ("Still found 'deleted' symbol, GOOG, in test watchlist: " ++ show (assets wl) ++ " ")
                    return False
                  else do
                    putStrLn "Successfully deleted Google."
                    return True


googleReq :: WatchlistSymbolPost
googleReq = WatchlistSymbolPost "GOOG"

testWL :: WatchlistPost
testWL = WatchlistPost "flowers" ["FLWS"]

checkForSym :: Maybe [Asset] -> String -> Bool
checkForSym Nothing _ = False
checkForSym (Just []) _ = False
checkForSym (Just (x : xs)) y = if Ast.symbol x == (pack y)
  then True
  else checkForSym (Just xs) y


toggleWatchSym
        :: CriaClient
        -> String
        -> String
        -> Bool
        -> IO (Either CriaError Watchlist)

toggleWatchSym cli wlID sym True = do
  res <- runReq cli $ signReq cli deleteSymbolWatchlist wlID sym
  return res

toggleWatchSym cli wlID sym False = do
  res <- runReq cli $ signReq cli addSymbolWatchlist wlID (WatchlistSymbolPost (pack sym))
  return res

testGetWatchlists :: CriaClient -> IO ()
testGetWatchlists cli = do
  wRes <- signAndRun cli getWatchlists
  case wRes of
    Left err -> do
      printWLError $ "Error: " ++ show err

    Right wl -> do
      let x:xs = wl
      print x
      print xs
