module Web.Neptune (
      module Data.Maybe
    , module Data.Monoid
    , module Control.Applicative
    , module Control.Monad
    
    , IsString(fromString)
    , ByteString, LByteString
    , toStrict, fromStrict
    , Text, LText
    , toStrictT, fromStrictT
    , Builder

    , Map, Vault, Key
    , newKey

    , module Web.Neptune.Types
    , Neptune, NeptuneM
    , module Web.Neptune.Route
    , module Web.Neptune.Action
    , module Web.Neptune.Format
    , module Web.Neptune.AltResponse
    ) where


import System.IO.Unsafe
import Data.ByteString.Lazy (toStrict, fromStrict)
import qualified Data.Text.Lazy as LT
import qualified Data.Vault.Lazy as Vault

import Data.Maybe
import Data.Monoid
import Control.Applicative
import Control.Monad

import Web.Neptune.Util
import Web.Neptune.Types
import Web.Neptune.Core
import Web.Neptune.Route
import Web.Neptune.Action
import Web.Neptune.Format
import Web.Neptune.AltResponse --TODO rename to Escape, or something

toStrictT = LT.toStrict
fromStrictT = LT.fromStrict

newKey :: Key a
newKey = unsafePerformIO Vault.newKey