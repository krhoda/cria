{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    ( someFunc
    ) where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import Servant.Types.SourceT (foreach)

import qualified Servant.Client.Streaming as S

-- import Control.Applicative
-- import Control.Monad
-- import Control.Monad.IO.Class
-- import Control.Monad.Trans.Except

-- import Data.Aeson
-- import Data.Monoid
-- import Data.Proxy
-- import Data.Text (Text)
-- import GHC.Generics
-- import Servant.API
-- import Servant.Client

-- import qualified Data.Text    as T
-- import qualified Data.Text.IO as T

someFunc :: IO ()
someFunc = putStrLn "someFunc"
