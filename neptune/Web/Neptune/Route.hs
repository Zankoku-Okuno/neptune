module Web.Neptune.Route (
      Route, Router, RouterM, Reverse, ReverseM
    , endpoint
    , external
    , include
    , cluster
    -- route combinators
    , zero
    , orRoute
    , literal
    , capture
    , remaining
    -- low-level route matching combinators
    , consume
    , DatumMonad(datum)
    , setDatum
    , noMatch
    , RequestMonad(request)
    , ConfigMonad(config)
    -- low-level route reversing combinators
    , create
    , creates
    , getArg
    , setDomain
    ) where

import Web.Neptune.Core

import Data.List.Split (wordsBy)
import qualified Data.Vault.Lazy as Vault
import qualified Data.Text as T
import qualified Data.Map as Map

import Data.Monoid
import Control.Monad.Reader
import Control.Monad.State

{- These are for adding routes to a neptune. -}
endpoint :: EndpointId -> Verb -> Route -> Action -> Neptune
endpoint eid m (R fore back) a = Neptune $ modify $ \s -> s
    { nHandlers = nHandlers s ++ [Endpoint fore m a]
    , nReversers = softInsert eid back (nReversers s)
    }

external :: EndpointId -> Domain -> Reverse -> Neptune
external eid domain back = Neptune $ modify $ \s -> s
    { nReversers = softInsert eid (back >> setDomain domain) (nReversers s) }

include :: Route -> Neptune -> Neptune
include (R fore back) neptune = Neptune $ do
    built <- flip buildSubNeptune neptune . nConfig <$> get
    modify $ \s -> s
        { nHandlers = nHandlers s ++ [Include fore (nHandlers built)]
        , nReversers = foldr (\(eid, back') -> softInsert eid (back >> back'))
                             (nReversers s)
                             (Map.toList $ nReversers built)
        }

cluster :: EndpointId -> Route -> [(Verb, Action)] -> Neptune
cluster eid (R fore back) actions = Neptune $ modify $ \s -> s
    { nHandlers = nHandlers s ++ [Include fore $ map (uncurry mkHandler) actions]
    , nReversers = softInsert eid back (nReversers s)
    }
    where
    mkHandler = Endpoint (return ())


{- Below is all about builting routes. -}
orRoute :: Route -> Route -> Route
(R fore1 back1) `orRoute` (R fore2 back2) = R fore back
    where
    fore = Router $ do
        s <- get
        m_result1 <- lift . lift $ runRouteM s fore1
        case m_result1 of
            Just success -> put success
            Nothing -> do
                m_result2 <- lift . lift $ runRouteM s fore2
                case m_result2 of
                    Just success -> put success
                    Nothing -> lift nothing
    back = Reverse $ do
        s <- ask
        case runReverseM s back1 of
            Just success -> lift $ put success
            Nothing -> case runReverseM s back2 of
                Just success -> lift $ put success
                Nothing -> lift . lift $ Nothing

remaining :: Key [Text] -> Route
remaining key = R fore back
    where
    fore = do
        l <- length . rPath <$> Router get
        when (l < 1) noMatch
        setDatum key =<< consume l
    back = getArg key >>= creates


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

instance DatumMonad RouterM where
    datum key = Router $ do
        vault <- rData <$> get
        return $ key `Vault.lookup` vault

setDatum :: Key a -> a -> Router
setDatum key x = Router $ do
    RS { rData = vault } <- get
    let vault' = Vault.insert key x vault
    modify $ \s -> s { rData = vault' }

{-| Always fails to match. -}
noMatch :: RouterM a
noMatch = Router $ lift nothing

instance RequestMonad RouterM where
    request = rRequest <$> Router get

instance ConfigMonad RouterM where
    config key = Vault.lookup key . nConfig . rNeptune <$> Router get


{-| Appends a path segment. -}
create :: Text -> Reverse
create = creates . (:[])
{-| Appends a path. -}
creates :: [Text] -> Reverse
creates xs = Reverse . lift . modify $ \(dom, path) -> (dom, path ++ xs)

{-| Retrives a parameter from the input. Fails if the key is not present. -}
getArg :: Key a -> ReverseM a
getArg key = Reverse $ lift . lift . Vault.lookup key . fst =<< ask

instance DatumMonad ReverseM where
    datum key = Reverse $ Vault.lookup key <$> asks fst


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
        setDatum key param
    back = create =<< f' <$> getArg key

captureIO :: (Text -> IO (Maybe a), a -> Text) -> Key a -> Route
captureIO (f, f') key = R fore back
    where
    fore = do
        [segment] <- consume 1
        param <- fromMaybeM noMatch $ liftIO $ f segment
        setDatum key param
    back = create =<< f' <$> getArg key

setDomain :: Domain -> Reverse
setDomain domain = Reverse $ modify $ \(_, path) -> (Just domain, path)

instance ConfigMonad ReverseM where
    config key = Reverse $ asks (Vault.lookup key . nConfig . snd)
