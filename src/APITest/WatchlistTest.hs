{-# LANGUAGE OverloadedStrings #-}

module APITest.WatchlistTest
    ( runWatchlistTest
    ) where

import Cria

import Record.Asset as Ast
import Record.Watchlist as Wl

import Record.Req.WatchlistPost
import Record.Req.WatchlistSymbolPost

import Data.Text (Text, unpack)

printWLError :: String -> IO ()
printWLError x = print (x ++ " Ending Watchlist Test...")

runWatchlistTest :: CriaClient -> IO ()
runWatchlistTest cli = do
  putStrLn "Checking Existing Watchlists\n"
  testGetWatchlists cli
  putStrLn "Making New Watchlist\n"
  eitherWL <- makeTestWatchlist cli
  checkTestWatchlist eitherWL

makeTestWatchlist :: CriaClient -> IO (Either CriaError Watchlist)
makeTestWatchlist cli = runReq cli $ (signReq cli createWatchlist) testWL

checkTestWatchlist :: Either CriaError Watchlist -> IO ()
checkTestWatchlist (Left err) =
        printWLError $ "Cannot Create Watchlist! Error: " ++ show err

checkTestWatchlist (Right wlList) = do
  print wlList
  -- putStrLn "Verify it does not have Google..."
  -- shouldBeFalse <- return checkForSym wl "GOOG"
  -- if shouldBeFalse
  --   then printWLError "Found google in test watchlist. Should only find FLWS."
  --   else do
  --     putStrLn "Verify it has flowers..."
  --     shouldBeTrue <- return checkForSym wl "FLWS"
  --     if (not shouldBeTrue)
  --       then printWLError "Found google in test watchlist. Should only find FLWS."
  --       else printWLError "Programmer didn't finish!"

googleReq :: WatchlistSymbolPost
googleReq = WatchlistSymbolPost "GOOG"

testWL :: WatchlistPost
testWL = WatchlistPost "flowers" ["FLWS"]

-- testWatchlistRoutes :: CriaClient -> Int -> IO Int
-- testWatchlistRoutes cli step = do
--   (nextStep, watchlistID) <- testGetWatchlists cli step
--   case watchlistID of
--     Nothing -> do
--       putStrLn "No ID returned stopping Watchlist tests..."
--         -- TODO: PUT CREATE HERE:
--       return nextStep
--     Just wlID -> do
--       lRes <- runReq cli (signReq cli getWatchlist (unpack wlID))
--       anotherStep <- testPrefix step "Testing Get Watchlist... About to toggle google"
--       testToggleWatchlistGoogle cli lRes
--       finalStep <-  testPrefix step "Toggled Google"

--       return finalStep

-- testToggleWatchlistGoogle :: CriaClient -> Either CriaError Watchlist -> IO ()
-- testToggleWatchlistGoogle _ (Left x) = print $ "Error: " ++ show x
-- testToggleWatchlistGoogle cli (Right wl) = do
--   toggleWatchingGoogle cli (unpack (watchlist_id wl)) (isWatchingGoogle (assets wl))

--         -- finalStep <- toggleWatchingGoogle cli nextStep (isWatchingGoogle asts)
--         -- return finalStep

-- isWatchingGoogle :: Maybe [Asset] -> Bool
-- isWatchingGoogle Nothing = False
-- isWatchingGoogle (Just []) = False
-- isWatchingGoogle (Just (x : xs)) = case Ast.symbol x of
--   "GOOG"  -> True
--   _ -> isWatchingGoogle (Just xs)


-- toggleWatchingGoogle :: CriaClient -> String -> Bool -> IO ()
-- toggleWatchingGoogle cli wlID True = do
--   res <- runReq cli $ signReq cli deleteSymbolWatchlist wlID "GOOG"
--   handleSimpleRes res print

-- toggleWatchingGoogle cli wlID False = do
--   res <- runReq cli $ signReq cli addSymbolWatchlist wlID googleReq
--   handleSimpleRes res print

testGetWatchlists :: CriaClient -> IO ()
testGetWatchlists cli = do
  wRes <- signAndRun cli getWatchlists
  -- nextStep <- testPrefix step "Testing List all Watchlists"
  case wRes of
    Left err -> do
      putStrLn $ "Error: " ++ show err

    Right wl -> do
      let x:xs = wl
      print x
      print xs
      -- return (nextStep, Just (Wl.watchlist_id $ head wl))

-- handleSimpleRes :: (Show a) => Either a b -> (b -> IO ()) -> IO ()
-- handleSimpleRes x f = do
--   case x of
--     Left y -> putStrLn $ "Error: " ++ show y
--     Right z ->  f z

-- handleTestRes :: (Show a) => Either a b -> (a -> IO c) -> (b -> IO c) -> IO c
-- handleTestRes x f g = do
--   case x of
--     Left y -> f y
--     Right z ->  g z
