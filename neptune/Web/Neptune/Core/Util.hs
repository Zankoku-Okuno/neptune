{-| This module combines many common imports and adds a few extras on top of them. -}
module Web.Neptune.Core.Util (
    -- * Modules
      module Data.Time.Clock
    , module Data.Maybe
    , module Data.Either
    , module Data.Monoid
    , module Control.Applicative
    , module Control.Monad
    , module Control.Monad.Trans
    -- * String Processing
    , Builder
    , ByteString
    , Text
    , LByteString
    , LText
    , IsString(..)
    -- * Containers
    , Map, Vault, Key
    , softInsert
    -- * Maybe Monad
    , nothing
    , fromMaybeM
    -- * Path Manipulation
    , normalizePath
    ) where

import Data.Time.Clock

import Data.Maybe
import Data.Either
import Control.Monad.Maybe
import Data.Map (Map)
import Data.Vault.Lazy (Vault, Key)
import qualified Data.Map as M

import Data.String (IsString(..))
import Blaze.ByteString.Builder (Builder)
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy as LBS

import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Monad.Trans


{-| I don't like how strict and lazy text share the same name.
    It makes reading type errors annoying when using multiple libraries.
-}
type LText = LT.Text
{-| I don't like how strict and lazy byte strings share the same name.
    It makes reading type errors annoying when using multiple libraries.
-}
type LByteString = LBS.ByteString


{-| After a monadic action yielding a 'Maybe',
    perform an action on the 'Nothing' case,
    or extract the value from a 'Just' case.
-}
fromMaybeM :: (Monad m) => m a -> m (Maybe a) -> m a
fromMaybeM def x = maybe def return =<< x

{-| The 'MaybeT' monad's version of the 'Maybe' type's Nothing. -}
nothing :: (Monad m) => MaybeT m a
nothing = MaybeT $ return Nothing


{-| Insert into a map only when the map does not already contain something under the key. -}
softInsert :: (Ord k) => k -> v -> M.Map k v -> M.Map k v
softInsert key val mapping =
    if key `M.member` mapping
        then mapping
        else M.insert key val mapping

{-| Transform a path, treating @\/\/@, @\/.\/@ and @\/..\/@ specially.

    All @\/\/@ and @\/.\/@ sequences are removed from the output.
    If possible, a @\/..\/@ sequence deletes the previous path segment as
    it is removed. If there is no prior path segment, then the @\/..\/@ is
    simply removed without aditional deletion.

    Note that this normalization ensures that the resultant path cannot
    be used to traverse upwards, out of a containing directory. Beware
    your filesystem, however, as symlinks can easily allow access to
    unexpected directories even under the conditions of this function.
-}
normalizePath :: [Text] -> [Text]
normalizePath = go []
    where
    go acc [] = reverse acc
    go acc ("":rest) = go acc rest
    go acc (".":rest) = go acc rest
    go [] ("..":rest) = go [] rest
    go (_:acc) ("..":rest) = go acc rest
    go acc (seg:rest) = go (seg:acc) rest

