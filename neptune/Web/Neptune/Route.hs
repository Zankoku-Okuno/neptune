module Web.Neptune.Route (
    -- * Types
      Route, Router, RouterM, Reverse, ReverseM
    -- * Create Resources
    , resource
    , endpoint
    , include
    , external
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

{-| Add an endpoint to a Neptune application.

    An \"endpoint\" is the intersection of a resource and a verb.
    Endpoints may be the entirety of a resource, but they are usually
    a sub-resource concept.
-}
endpoint :: EndpointId -> Verb -> Route -> Action -> Neptune
endpoint eid m (R fore back) a = Neptune $ modify $ \s -> s
    { nlHandlers = nlHandlers s ++ [Endpoint fore m a]
    , nlReversers = softInsert eid back (nlReversers s)
    }

{-| Add an external URL to a Neptune application.
    External URLs are used only for URL reversing.
-}
external :: EndpointId -> URL -> Reverse -> Neptune
external eid prepath back = Neptune $ modify $ \s -> s
    { nlReversers = softInsert eid (back >> setDomain prepath) (nlReversers s) }

{-| Include an existing Neptune application within a larger one.

    In large sites, 'include' is essential to ensure good routing performance.
    If all resources were added through 'endpoint', routing would take O(n) time
    in the number of resources. With appropriate use if 'include', this can be 
    reduced to O(log(n)) time.
-}
include :: Route -> Neptune -> Neptune
include (R fore back) neptune = Neptune $ do
    subNeptune <- flip buildNeptune neptune . nlConfig <$> get
    modify $ \s -> s
        { nlHandlers = nlHandlers s ++ [Include fore (nlHandlers subNeptune)]
        , nlReversers = foldr (\(eid, back') -> softInsert eid (back >> back'))
                             (nlReversers s)
                             (Map.toList $ nlReversers subNeptune)
        }

-- |Add a single resource with many available verbs.
resource :: EndpointId -> Route -> [(Verb, Action)] -> Neptune
resource eid (R fore back) actions = Neptune $ modify $ \s -> s
    { nlHandlers = nlHandlers s ++ [Include fore $ map (uncurry mkHandler) actions]
    , nlReversers = softInsert eid back (nlReversers s)
    }
    where
    mkHandler = Endpoint (return ())


-- |Match the first route, or else match the second.
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

-- |Comsume all remaining path segments and store under the passed 'Key'.
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

-- |Create/update the datum value stored under the passed 'Key'.
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


-- |Match an empty path.
zero :: Route
zero = mempty

{-| Match exactly with the passed 'Text'. If there are slashes in the
    literal, they are treated as path separators.
-}
literal :: Text -> Route
literal path = R fore back
    where
    segments = T.pack <$> wordsBy (=='/') (T.unpack path)
    fore = do
        segments' <- consume (length segments)
        when (segments' /= segments) noMatch
    back = creates segments

{-| Attempt to parse the leading segment of the remaining path and store
    it under the passed 'Key'.
-}
capture :: (Text -> Maybe a, a -> Text) -- parsing (which can fail) and rendering functions
        -> Key a -> Route
capture (f, f') key = R fore back
    where
    fore = do
        [segment] <- consume 1
        param <- maybe noMatch return $ f segment
        setDatum key param
    back = create =<< f' <$> getArg key

-- |As 'capture', but parsing may use 'IO'.
--
--  It is not recommended that this function mutate any data, only read it.
captureIO :: (Text -> IO (Maybe a), a -> Text) -> Key a -> Route
captureIO (f, f') key = R fore back
    where
    fore = do
        [segment] <- consume 1
        param <- fromMaybeM noMatch $ liftIO $ f segment
        setDatum key param
    back = create =<< f' <$> getArg key


setDomain :: URL -> Reverse
setDomain prepath = Reverse $ modify $ \(_, path) -> (Just prepath, path)

instance ConfigMonad ReverseM where
    config key = Reverse $ asks (Vault.lookup key . nConfig . snd)
