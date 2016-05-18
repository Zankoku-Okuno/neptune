{-#LANGUAGE GADTs, ExistentialQuantification, TypeFamilies, PolyKinds, FlexibleInstances #-}
module Web.Neptune.Core.Uri where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Monoid
import Data.Foldable
import Control.Monad
import Control.Monad.Writer

type Bijection a b = (a -> b, b -> a)


data Match a
    = Match a
    | NoMatch
data Uri a where
    Simple :: ByteString -> Uri () -- ^ recognize a 'Uri' that simply equals the given literal
    Complex :: (Bijection ByteString (Match a)) -> Uri a -- ^ recognize a 'Uri' based on any algorithm, return true if it is a match
    Prefix :: ByteString -> (Bijection ByteString (Match a)) -> Uri a -- ^ recognize a 'Uri' by stripping a prefix, then applying an algorithm as in 'Complex'

addPrefix :: ByteString -> Uri a -> Uri a
addPrefix prefix (Simple uri) = Simple (prefix <> uri)
addPrefix prefix (Complex biject) = Prefix prefix biject
addPrefix prefix (Prefix old biject) = Prefix (prefix <> old) biject

match :: Uri a -> ByteString -> Match a
match (Simple uri) candidate = Match ()
match (Complex (recognize, _)) candidate = recognize candidate
match (Prefix prefix (recognize, _)) candidate =
    case BS.stripPrefix prefix candidate of
        Nothing -> NoMatch
        Just rest -> recognize rest



class Dispatchable t where
    data Dispatch t :: * -> *
    type DispatchOn t
    data ConcatError t

    add :: Dispatches t r -> Dispatch t r -> DispatchesM t r
    dispatch :: Dispatch t r -> DispatchOn t -> Maybe r

data Dispatches t r = Dispatches [Dispatch t r]
type DispatchesM t r = Writer [ConcatError t] (Dispatches t r)

dispatches :: (Dispatchable t) => Dispatches t r -> DispatchOn t -> Maybe r
dispatches (Dispatches ds) it = asum $ map (`dispatch` it) ds

addAll :: (Dispatchable t) => Dispatches t r -> [Dispatch t r] -> DispatchesM t r
addAll old new = foldM add old new

instance (Dispatchable t) => Monoid (DispatchesM t r) where
    mempty = pure $ Dispatches []
    mappend orig' new' = do
       (Dispatches orig) <- orig'
       (Dispatches new) <- new'
       pure (Dispatches $ orig <> new) 





instance Dispatchable Uri where
    data Dispatch Uri r = forall a. DUri (Uri a) (a -> r)
    type DispatchOn Uri = ByteString
    data ConcatError Uri = forall r. Overlap (Dispatch Uri r) (Dispatch Uri r)

    dispatch (DUri uri next) str = case uri `match` str of
        NoMatch -> Nothing
        Match a -> Just $ next a

    add (Dispatches old) new = Dispatches <$> go old
        where
        go [] = pure [new]
        go (x:xs) = do
            if isAmbiguous x new
                then old <$ tell [Overlap new x]
                else (x:) <$> go xs
        isAmbiguous (DUri a _) (DUri b _) = check a b
            where
            check (Simple a) (Simple b) = a == b
            check (Simple a) (Prefix b _) = b `BS.isPrefixOf` a
            check (Prefix a _) (Simple b) = a `BS.isPrefixOf` b
            check (Prefix a _) (Prefix b _) = a `BS.isPrefixOf` b || b `BS.isPrefixOf` a
            check _ _ = False


subapp :: Dispatches Uri r -> (ByteString, Dispatches Uri r) -> DispatchesM Uri r
subapp old (prefix, Dispatches new) = pure old <> (pure . Dispatches) (addPrefix' prefix <$> new)
    where addPrefix' prefix (DUri it next) = DUri (addPrefix prefix it) next

