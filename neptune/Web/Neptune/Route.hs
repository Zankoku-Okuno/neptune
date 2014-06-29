{-# LANGUAGE OverloadedStrings #-}
module Web.Neptune.Route (
      endpoint
    , external
    , include
    , cluster
    -- route combinators
    , zero
    , literal
    , capture
    -- low-level route matching combinators
    , consume
    , ParamMonad(param)
    , setParam
    , noMatch
    , RequestMonad(request, requests)
    -- low-level route reversing combinators
    , create
    , creates
    , getArg
    , setDomain
    ) where

import Web.Neptune.Util
import Web.Neptune.Core

import Data.List.Split (wordsBy)
import qualified Data.Vault.Lazy as Vault
import qualified Data.Text as T
import qualified Data.Map as Map

import Data.Monoid
import Control.Monad.Reader
import Control.Monad.State

{- These are for adding routes to a neptune. -}
endpoint :: EndpointId -> Route -> Method -> Action -> Neptune
endpoint eid (R fore back) m a = Neptune $ modify $ \s -> s
    { nHandlers = nHandlers s ++ [Endpoint fore m a]
    , nReversers = softInsert eid back (nReversers s)
    }

external :: EndpointId -> Domain -> Reverse -> Neptune
external eid domain back = Neptune $ modify $ \s -> s
    { nReversers = softInsert eid (back >> setDomain domain) (nReversers s) }

include :: Route -> Neptune -> Neptune
include (R fore back) neptune = Neptune $ modify $ \s -> s
    { nHandlers = nHandlers s ++ [Include fore (nHandlers built)]
    , nReversers = foldr (\(eid, back') -> softInsert eid (back >> back'))
                         (nReversers s)
                         (Map.toList $ nReversers built)
    }
    where
    built = buildNeptune undefined neptune

cluster :: EndpointId -> Route -> [(Method, Action)] -> Neptune
cluster eid (R fore back) actions = Neptune $ modify $ \s -> s
    { nHandlers = nHandlers s ++ [Include fore $ map (uncurry mkHandler) actions]
    , nReversers = softInsert eid back (nReversers s)
    }
    where
    mkHandler = Endpoint (return ())


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

instance ParamMonad RouterM where
    param key = Router $ do
        vault <- rParams <$> get
        return $ key `Vault.lookup` vault

{-| Adds a parameter to the result. -}
setParam :: Key a -> a -> Router
setParam key x = Router $ do
    RS { rParams = vault } <- get
    let vault' = Vault.insert key x vault
    modify $ \s -> s { rParams = vault' }

{-| Always fails to match. -}
noMatch :: RouterM a
noMatch = Router $ lift nothing

instance RequestMonad RouterM where
    request = Router $ rRequest <$> get


{-| Appends a path segment. -}
create :: Text -> Reverse
create = creates . (:[])
{-| Appends a path. -}
creates :: [Text] -> Reverse
creates xs = Reverse . lift . modify $ \(dom, path) -> (dom, path ++ xs)

{-| Retrives a parameter from the input. Fails if the key is not present. -}
getArg :: Key a -> ReverseM a
getArg key = Reverse $ lift . lift . Vault.lookup key =<< ask


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
    back = create =<< f' <$> getArg key

captureIO :: (Text -> IO (Maybe a), a -> Text) -> Key a -> Route
captureIO (f, f') key = R fore back
    where
    fore = do
        [segment] <- consume 1
        param <- fromMaybeM noMatch $ liftIO $ f segment
        setParam key param
    back = create =<< f' <$> getArg key

setDomain :: Domain -> Reverse
setDomain domain = Reverse $ modify $ \(_, path) -> (Just domain, path)