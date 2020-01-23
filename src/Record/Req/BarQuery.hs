module Record.Req.BarQuery where

import Record.Bar
import Servant.Client (ClientM)
import Data.List (intercalate)

-- helpers for Bars request
data BarTimeframe = Min1 | Min5 | Min15 | Day1

barTimeframeToString :: BarTimeframe -> String
barTimeframeToString Min1 = "1Min"
barTimeframeToString Min5 = "5Min"
barTimeframeToString Min15 = "15Min"
barTimeframeToString Day1 = "Day1"

data BarStartOrAfter = BarStart | BarAfter

data BarEndOrUntil = BarEnd | BarUntil

data BarQuery = BarQuery {timeframe :: BarTimeframe
                         ,symbols :: [String]
                         ,barLimit :: Maybe Integer
                         ,startOrAfter :: Maybe (BarStartOrAfter, String)
                         ,endOrUntil :: Maybe (BarEndOrUntil, String)
                         }

barQueryToReq :: BarQuery -> Either String (String, Maybe String, Maybe Integer, Maybe String, Maybe String, Maybe String, Maybe String)
barQueryToReq (BarQuery tf syms mayLim staraft endtil) =
  if (length syms == 0 || length syms > 200)
     then Left "Symbols must be between 1 and 200"
     else
       case (formatLimit mayLim) of
          Left x -> Left x
          Right limQuery -> Right ((barTimeframeToString tf)
                                  ,Just (intercalate "," syms)
                                  ,limQuery
                                  ,(handleStart staraft)
                                  ,(handleEnd endtil)
                                  ,(handleAfter staraft)
                                  ,(handleUntil endtil)
                                  )
fmtLimitString = "Limit must be between 1 and 1000 (inclusive). Pass nothing for API default of 100."

formatLimit :: Maybe Integer -> Either String (Maybe Integer)
formatLimit Nothing = Right Nothing
formatLimit (Just x) = if x <= 0 || x > 1000
  then Left fmtLimitString
  else Right (Just x)

handleStart :: Maybe (BarStartOrAfter, String) -> Maybe String
handleStart Nothing = Nothing
handleStart (Just (BarAfter, _)) = Nothing
handleStart (Just (BarStart, target)) = Just target

handleAfter :: Maybe (BarStartOrAfter, String) -> Maybe String
handleAfter Nothing = Nothing
handleAfter (Just (BarStart, _)) = Nothing
handleAfter (Just (BarAfter, target)) = Just target

handleEnd :: Maybe (BarEndOrUntil, String) -> Maybe String
handleEnd Nothing = Nothing
handleEnd (Just (BarUntil, _)) = Nothing
handleEnd (Just (BarEnd, target)) = Just target

handleUntil :: Maybe (BarEndOrUntil, String) -> Maybe String
handleUntil Nothing = Nothing
handleUntil (Just (BarEnd, _)) = Nothing
handleUntil (Just (BarUntil, target)) = Just target
