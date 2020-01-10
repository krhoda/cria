{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Asset where

import Data.Aeson
import Data.Aeson.TH (deriveJSON, defaultOptions, Options(fieldLabelModifier))
import Data.Text (Text)

import GHC.Generics (Generic)

data Asset = Asset {
    marginable :: Bool,
    status :: Text,
    shortable :: Bool,
    exchange :: Text,
    symbol :: Text,
    id :: Text,
    easy_to_borrow :: Bool,
    asset_class :: Text, -- TODO: Write custom parser.
    tradable :: Bool
} deriving (Show,Eq,GHC.Generics.Generic)

$(deriveJSON defaultOptions {fieldLabelModifier = \x ->
                                if x == "asset_class"
                                then "class"
                                else x} ''Asset)

-- instance FromJSON Asset
