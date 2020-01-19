module APITest.AccountTest
    ( runAccountTest
    ) where

import Cria

import Record.Alparseable
import Record.Account as Acct

runAccountTest :: CriaClient -> IO ()
runAccountTest cli = do
  putStrLn "Testing Get Account:"
  putStrLn ""
  res <- signAndRun cli getAccount
  case res of
    Left err -> do
      putStrLn $ "Error: " ++ show err

    Right acct -> do
      print acct
      putStrLn "Testing Alparse, Expecting Nothing Just <number> Just <number>"
      putStrLn ""
      print (alparse (Acct.status acct)) -- Will Print Nothing.
      print (alparse (buying_power acct))
      print (alparse (cash acct))
