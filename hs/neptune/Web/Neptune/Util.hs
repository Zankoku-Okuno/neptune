module Web.Neptune.Util (
	  module X
	, Text
	, nothing
	, fromMaybeM
	, softInsert
	) where

import Control.Monad.Maybe
import qualified Data.Map as M


import Data.Maybe as X
import Data.Text (Text)

import Control.Applicative as X
import Control.Monad as X

fromMaybeM :: (Monad m) => m a -> m (Maybe a) -> m a
fromMaybeM def x = maybe def return =<< x

nothing :: (Monad m) => MaybeT m a
nothing = MaybeT $ return Nothing

softInsert :: (Ord k) => k -> v -> M.Map k v -> M.Map k v
softInsert key val map =
    if key `M.member` map
        then map
        else M.insert key val map
