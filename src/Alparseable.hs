module Alparseable where

import Data.Text (Text, unpack)
import Text.Read (readMaybe)

type Alparseable = Text

alparse :: Alparseable -> Maybe Double
alparse x = readMaybe (unpack x) :: Maybe Double
