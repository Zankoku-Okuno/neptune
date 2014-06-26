{-# LANGUAGE OverloadedStrings #-}
module Web.Neptune.Route (
      endpoint
    , include
    -- route combinators
    , zero
    , literal
    , capture
    -- low-level route matching combinators
    , consume
    , setParam
    , noMatch
    -- low-level route reversing combinators
    , create
    , creates
    , getParam
    ) where

import Data.List.Split (wordsBy)
import Data.Vault.Lazy (Key)
import qualified Data.Vault.Lazy as Vault
import qualified Data.Text as T
import qualified Data.Map as Map

import Web.Neptune.Util
import Web.Neptune.Core

import Data.Monoid
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

{- These are for adding routes to a neptune. -}
endpoint :: EndpointId -> Route -> Method -> Action -> Neptune
endpoint eid (R fore back) m a = NeptuneM $ modify $ \s -> s
    { nHandlers = nHandlers s ++ [Endpoint fore m a]
    , nReversers = softInsert eid back (nReversers s)
    }

include :: Route -> Neptune -> Neptune
include (R fore back) neptune = NeptuneM $ modify $ \s -> s
    { nHandlers = nHandlers s ++ [Include fore (nHandlers built)]
    , nReversers = foldr (\(eid, back') -> softInsert eid (back >> back'))
                         (nReversers s)
                         (Map.toList $ nReversers built)
    }
    where
    built = buildNeptune neptune


{- Below is all about builting routes. -}
{-| Remove the passed number of segments from the front of the remaining path.
    Fails if the remaining path is not long enough.
-}
consume :: Int -> RouterM [Text]
consume n = Router $ do
    RS { rPath = path } <- get
    when (length path < n) $ lift nothing
    let (prefix, suffix) = splitAt n path
    modify $ \s -> s { rPath = suffix }
    return prefix

{-| Adds a parameter to the result. -}
setParam :: Key a -> a -> Router
setParam key x = Router $ do
    RS { rParams = vault } <- get
    let vault' = Vault.insert key x vault
    modify $ \s -> s { rParams = vault' }

{-| Always fails to match. -}
noMatch :: RouterM a
noMatch = Router $ lift nothing


{-| Appends a path segment. -}
create :: Text -> Reverse
create = creates . (:[])
{-| Appends a path. -}
creates :: [Text] -> Reverse
creates = Reverse . lift . tell

{-| Retrives a parameter from the input. Fails if the key is not present. -}
getParam :: Key a -> ReverseM a
getParam key = Reverse $ lift . lift . Vault.lookup key =<< ask


{-| Routes can be added together piece-by-piece. -}
instance Monoid Route where
    mempty = R (return ()) (return ())
    (R fore1 back1) `mappend` (R fore2 back2) = R (fore1 >> fore2) (back1 >> back2)


zero :: Route
zero = mempty

literal :: Text -> Route
literal path = R fore back
    where
    segments = T.pack <$> wordsBy (=='/') (T.unpack path)
    fore = do
        segments' <- consume (length segments)
        when (segments' /= segments) noMatch
    back = creates segments

capture :: (Text -> Maybe a, a -> Text) -> Key a -> Route
capture (f, f') key = R fore back
    where
    fore = do
        [segment] <- consume 1
        param <- maybe noMatch return $ f segment
        setParam key param
    back = create =<< f' <$> getParam key

captureIO :: (Text -> IO (Maybe a), a -> Text) -> Key a -> Route
captureIO (f, f') key = R fore back
    where
    fore = do
        [segment] <- consume 1
        param <- fromMaybeM noMatch $ liftIO $ f segment
        setParam key param
    back = create =<< f' <$> getParam key