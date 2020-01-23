{-# LANGUAGE DeriveGeneric #-}

module Record.Bar where

import Data.Aeson
import Data.Map
import Data.Text (Text)

import GHC.Generics (Generic)

data Bar =  Map Text [BarEntry] deriving (Show, Eq, Generic)
instance FromJSON Bar

data BarEntry = BarEntry {
 t :: Float
 ,o :: Float
 ,h :: Float
 ,l :: Float
 ,c :: Float
 ,v :: Float
 } deriving (Show, Eq, Generic)

instance FromJSON BarEntry
