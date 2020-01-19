{-# LANGUAGE OverloadedStrings #-}

module APITest.AccountTest
    ( runAccountTest
    ) where

import Cria

import Record.Alparseable
import Record.Account as Acct
import Record.AccountConfiguration

runAccountTest :: CriaClient -> IO ()
runAccountTest cli = do
  putStrLn "Testing Get Account:"
  putStrLn ""
  res <- signAndRun cli getAccount
  case res of
    Left err -> do
      putStrLn $ "Error: " ++ show err
    Right acct -> do
      printAccountAndTestAlparseable acct
      configRes <- signAndRun cli getAccountConfig
      testConfig cli configRes

      -- TODO: ADD ACCOUNT ACTIVITIES ONCE I HAVE TESTED THEM.
      -- THESE TWO ROUTES: https://docs.alpaca.markets/api-documentation/api-v2/account-activities/


testConfig :: CriaClient ->  Either CriaError AccountConfiguration -> IO ()
testConfig _ (Left err) = putStrLn $ "Failed to get config: " ++ show err

testConfig cli (Right ac) = do
  roundTripConfigBools cli ac
  roundTripConfigDTBP cli ac

roundTripConfigBools :: CriaClient -> AccountConfiguration -> IO ()
roundTripConfigBools cli ac = do
  let nextAC = AccountConfiguration
                 (dtbp_check ac)
                 (not (no_shorting ac))
                 (not (suspend_trade ac))
                 (trade_confirm_email (swapConfigEmail ac))

  putStrLn $ "About to flip\n" ++ show ac ++ "\nto\n" ++ show nextAC ++ "\n"
  -- flipped <- runReq cli $ (signReq cli updateAccountConfig) nextAC
  flipped <- signAndRun cli $ updateAccountConfig nextAC
  -- flipped <- signAndRun cli
  case flipped of
    Left err -> putStrLn $ "Failed in flip request " ++ show err

    Right flippedAC -> do
      if ((no_shorting ac) == not (no_shorting flippedAC)
        && suspend_trade ac == not (suspend_trade flippedAC)
        && trade_confirm_email ac == trade_confirm_email (swapConfigEmail flippedAC))

        then do
          putStrLn "Finished flipped, about to unflip"
          unflipped <- signAndRun cli $ updateAccountConfig ac

          case unflipped of
            Left err -> putStrLn $ "Failed in unflipped" ++ show err
            Right unflippedAC -> putStrLn $ "Round Trip Successful? " ++ show (unflippedAC == ac)

        else putStrLn "Failed to validate flipped config"

roundTripConfigDTBP :: CriaClient -> AccountConfiguration -> IO ()
roundTripConfigDTBP cli ac = do
  let nextACReq = swapConfigDTBP ac
  nextACRes <- signAndRun cli $ updateAccountConfig nextACReq
  case nextACRes of
    Left err -> do
      putStrLn $ "Failed in first shift: " ++ show err
    Right nextAC -> do
      if not (nextAC == swapConfigDTBP ac)
        then do
          putStrLn $ "Failed in equality in first shift: \n" ++ show nextACRes ++ "\n"
        else do
          lastACReq <- return (swapConfigDTBP nextAC)
          lastACRes <- signAndRun cli $ updateAccountConfig lastACReq

          case lastACRes of
            Left err -> do
              putStrLn $ "Failed in first shift: " ++ show err
            Right lastAC -> do
              if not (lastAC == swapConfigDTBP nextAC)
                then do
                  putStrLn $ "Failed in equality in second shift: \n" ++ show lastACRes ++ "\n"
                 else do

                  origACRes <- signAndRun cli $ updateAccountConfig ac
                  case origACRes of
                    Left err -> do
                      putStrLn $ "Failed in last shift: " ++ show err

                    Right origAC -> do
                      if not (origAC == swapConfigDTBP lastAC)
                        then do
                          putStrLn $ "Failed in equality last shift: \n" ++ show lastAC ++ "\n"
                        else do
                          putStrLn "Succeed in DTBP Round trip"

swapConfigEmail :: AccountConfiguration -> AccountConfiguration
swapConfigEmail ac = case trade_confirm_email ac of
  "all" -> AccountConfiguration (dtbp_check ac) (no_shorting ac) (suspend_trade ac) "none"
  "none" -> AccountConfiguration (dtbp_check ac) (no_shorting ac) (suspend_trade ac) "all"
  _ -> ac

swapConfigDTBP :: AccountConfiguration -> AccountConfiguration
swapConfigDTBP ac = case trade_confirm_email ac of
  "both" -> AccountConfiguration "entry" (no_shorting ac) (suspend_trade ac) (trade_confirm_email ac)
  "entry" -> AccountConfiguration "exit" (no_shorting ac) (suspend_trade ac) (trade_confirm_email ac)
  "exit" -> AccountConfiguration "both" (no_shorting ac) (suspend_trade ac) (trade_confirm_email ac)
  _ -> ac

printAccountAndTestAlparseable :: Account -> IO ()
printAccountAndTestAlparseable acct = do
  print acct
  putStrLn "Testing Alparse, Expecting Nothing Just <number> Just <number>"
  putStrLn ""
  print (alparse (Acct.status acct)) -- Will Print Nothing.
  print (alparse (buying_power acct))
  print (alparse (cash acct))
