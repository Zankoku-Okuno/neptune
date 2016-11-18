{-#LANGUAGE TypeOperators #-}
module Web.Neptune.Util.Partial
    ( type (->?)(..), ($?)
    , module Control.Applicative
    , module Control.Category
    ) where

import Prelude hiding (id, (.))

import Data.Maybe
import Control.Applicative
import Control.Category


newtype a ->? b = PF (a -> Maybe b)

infixr 0 $?
($?) :: (a ->? b) -> a -> Maybe b
(PF f) $? x = f x


instance Functor ((->?) a) where
    f `fmap` (PF t) = PF $ \val -> f <$> t val
instance Applicative ((->?) a) where
    pure = PF . const . Just
    (PF a) <*> (PF b) = PF $ \val -> a val <*> b val
instance Alternative ((->?) a) where
    empty = PF $ const Nothing
    (PF a) <|> (PF b) = PF $ \val -> a val <|> b val

instance Category (->?) where
    id = PF $ Just
    (PF b) . (PF a) = PF $ \val -> b =<< a val
