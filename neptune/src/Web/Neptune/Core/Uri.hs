{-#LANGUAGE GADTs, ExistentialQuantification, TypeFamilies, PolyKinds, FlexibleInstances, UndecidableInstances #-}
module Web.Neptune.Core.Uri
    ( Uri(..)
    , Dispatch, fromList, DispatchM
    , dispatch
    , subapp
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Monoid
import Data.Foldable
import Control.Monad
import Control.Monad.Writer

type Bijection a b = (a -> b, b -> a)



class Uri t where
    data Matcher t :: * -> *
    data ConcatError t

    -- if 'x `overlaps` y == False', then for all 'a', 'isJust $ x `match` a' implies 'isNothing $ y `match` a'
    match :: Matcher t r -> t -> Maybe r
    -- overlaps is commutative
    overlaps :: Matcher t r -> Matcher t r -> Bool

data Dispatch t r = Dispatch_ [Matcher t r]
type DispatchM t r = Writer [ConcatError t] (Dispatch t r)

fromList :: (Uri uri, Traversable t) => t (Matcher uri r) -> Dispatch uri r
fromList xs = Dispatch_ $ foldr (:) [] xs

dispatch :: (Uri t) => Dispatch t r -> t -> Maybe r
dispatch (Dispatch_ ds) it = asum $ map (`match` it) ds

addAll :: (Uri t) => Dispatch t r -> [Matcher t r] -> DispatchM t r
addAll old new = foldM add old new

add :: (Uri t) => Dispatch t r -> Matcher t r -> DispatchM t r
add (Dispatch_ old) new = Dispatch_ <$> go old
    where
    go [] = pure [new]
    go (x:xs) = do
        when (new `overlaps` x) $ tell [Overlap new x]
        (x:) <$> go xs

instance (Uri t) => Monoid (DispatchM t r) where
    mempty = pure $ Dispatch_ []
    mappend orig' new' = do
       (Dispatch_ orig) <- orig'
       (Dispatch_ new) <- new'
       pure (Dispatch_ $ orig <> new) 




class (Eq a, Monoid a) => Prefix a where
    isPrefixOf :: a -> a -> Bool
    stripPrefix :: a -> a -> Maybe a

instance Prefix ByteString where
    isPrefixOf = BS.isPrefixOf
    stripPrefix = BS.stripPrefix

data PrefixMatcher str a where
    Simple :: str -> PrefixMatcher str () -- ^ recognize a uri that simply equals the given literal
    Complex :: (Bijection str (Maybe a)) -> PrefixMatcher str a -- ^ recognize a uri based on any algorithm, return true if it is a match
    Prefix :: str -> (Bijection str (Maybe a)) -> PrefixMatcher str a -- ^ recognize a uri by stripping a prefix, then applying an algorithm as in 'Complex'

addPrefix :: (Prefix str) => str -> PrefixMatcher str a -> PrefixMatcher str a
addPrefix prefix (Simple uri) = Simple (prefix <> uri)
addPrefix prefix (Complex biject) = Prefix prefix biject
addPrefix prefix (Prefix old biject) = Prefix (prefix <> old) biject


instance (Prefix str) => Uri str where
    data Matcher str r = forall a. DUri (PrefixMatcher str a) (a -> r)
    data ConcatError str = forall r. Overlap (Matcher str r) (Matcher str r)

    match (DUri (Simple uri) next) candidate = pure $ next ()
    match (DUri (Complex (recognize, _)) next) candidate = next <$> recognize candidate
    match (DUri (Prefix prefix (recognize, _)) next) candidate = next <$> (stripPrefix prefix candidate >>= recognize)


    overlaps (DUri a _) (DUri b _) = check a b
        where
        check (Simple a) (Simple b) = a == b
        check (Simple a) (Prefix b _) = b `isPrefixOf` a
        check (Prefix a _) (Simple b) = a `isPrefixOf` b
        check (Prefix a _) (Prefix b _) = a `isPrefixOf` b || b `isPrefixOf` a
        check _ _ = False


subapp :: (Prefix str) => Dispatch str r -> (str, Dispatch str r) -> DispatchM str r
subapp old (prefix, Dispatch_ new) = pure old <> (pure . Dispatch_) (addPrefix' prefix <$> new)
    where addPrefix' prefix (DUri it next) = DUri (addPrefix prefix it) next

