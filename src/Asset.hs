{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module Asset where

import Data.Text (Text)
import qualified GHC.Generics

data Asset = Asset {
    marginable :: Bool,
    status :: Text,
    shortable :: Bool,
    exchange :: Text,
    symbol :: Text,
    id :: Text,
    easy_to_borrow :: Bool,
    -- class :: Text, TODO: FIGURE OUT HOW TO HANDLE THIS!
    tradable :: Bool
  } deriving (Show,Eq,GHC.Generics.Generic)
