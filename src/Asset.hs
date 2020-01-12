{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Asset where

import Data.Aeson
import Data.Aeson.TH (deriveJSON, defaultOptions, Options(fieldLabelModifier))
import Data.Text (Text)

import GHC.Generics (Generic)

data Asset = Asset {
    asset_class :: Text,
    easy_to_borrow :: Bool,
    exchange :: Text,
    asset_id :: Text,
    marginable :: Bool,
    shortable :: Bool,
    status :: Text,
    symbol :: Text,
    tradable :: Bool
    } deriving (Show, Eq, Generic)

$(deriveJSON defaultOptions {fieldLabelModifier = \x ->
                                case x of
                                    "asset_class" -> "class"
                                    "asset_id" -> "id"
                                    _ -> x} ''Asset)
