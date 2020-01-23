{-# LANGUAGE OverloadedStrings #-}

module APITest.AssetTest
    ( runAssetTest
    ) where

import Cria

import Record.Asset

import Data.Text (unpack)

runAssetTest :: CriaClient -> Bool -> IO ()
runAssetTest cli verbose = do
  putStrLn "About to test getAssetBySymbol"
  res1 <- signAndRun cli $ getAssetBySymbol "FLWS"
  case res1 of
    Left err -> putStrLn $ "Couldn't get asset1..." ++ show err
    Right asset1 -> do
      putStrLn "Success In Symbol:"
      print asset1
      res2 <- signAndRun cli $ getAssetByID $ unpack $ asset_id asset1
      case res2 of
        Left err -> putStrLn $ "Couldn't get asset2..." ++ show err
        Right asset2 -> do
          putStrLn "Success In ID:"
          print asset2
          if not (asset1 == asset2)
            then putStrLn "Asset 1 and Asset 2 do not match..."
            else do
              putStrLn "Now for the big one..."
              if verbose
                then do
                  res <- signAndRun cli $ getAssetList (Just "active") (Just "us_equity")
                  -- TODO: FINISH THIS:
                  print res
                else putStrLn "Verbose is off... skipping get all assets."
