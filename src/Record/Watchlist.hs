{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Record.Watchlist where

import Record.Asset

import Data.Aeson
import Data.Aeson.TH (deriveJSON, defaultOptions, Options (fieldLabelModifier))

import Data.Text (Text)

import GHC.Generics (Generic)

data Watchlist = Watchlist {
    account_id :: Text,
    assets :: Maybe [Asset],
    created_at :: Text,
    watchlist_id :: Text,
    name :: Text,
    updated_at :: Text
    } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions {fieldLabelModifier = \x ->
                                case x of
                                    "watchlist_id" -> "id"
                                    _ -> x,
                                    omitNothingFields = True} ''Watchlist)
