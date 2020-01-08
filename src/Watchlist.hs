
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}
module Watchlist where

import Data.Text (Text)
import qualified GHC.Generics

import Asset

data Watchlist = TopLevel {
    name :: Text,
    updated_at :: Text,
    created_at :: Text,
    id :: Text,
    account_id :: Text,
    assets :: [Asset]
  } deriving (Show,Eq,GHC.Generics.Generic)
