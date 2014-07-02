{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, RankNTypes #-}
module Web.Neptune.Core (
      module Web.Neptune.Core.Util
    , module Web.Neptune.Core.Types
    -- * Neptune Applications
    , Neptune
    , NeptuneM(..)
    , buildNeptune
    , buildSubNeptune
    , NeptuneState(..)
    , ErrorHandlers(..)
    -- * Result Monad
    , Result(..)
    , ResultT(..)
    , runResultT
    , ResultMonad(..)
    -- * Routing
    , Route(..)
    -- ** Forward Routing
    , Router
    , RouterM(..)
    , RoutingState(..)
    , runRouteM
    , runRoutesM
    , Reverse
    , ReverseM(..)
    , runReverseM
    -- * Handling
    , Handler(..)
    , HandlingState(..)
    -- ** Actions
    , Action
    , ActionM(..)
    , runActionM
    -- ** Formatting
    , Format
    , FormatM(..)
    , runFormatM
    -- * Convenience Monad Classes
    , RequestMonad(..)
    , DatumMonad(..)
    , ReverseMonad(..)
    , ConfigMonad(..)
    -- * Dispatch Processes
    , evalHandler
    , evalHandlers
    , negotiate
    , reverseUrl
    ) where

import Web.Neptune.Core.Util
import Web.Neptune.Core.Types

import Data.Word8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import qualified Data.Map as Map
import qualified Data.Vault.Lazy as Vault

import Control.Monad.Identity
import Control.Monad.Maybe
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as Wai
import qualified Network.HTTP.Types as Wai
import qualified Network.HTTP.Media as Wai
import qualified Web.Cookie as Wai


{-| Since 'NeptuneM' is essentially just an accumulator,
    this type should come up often for complete Neptune applications.
-}
type Neptune = NeptuneM ()

{-| The 'NeptuneM' monad accumulates all the application configuration.
    This includes routing, URL reversing, and error handlers.
-}
newtype NeptuneM a = Neptune { unNeptune :: State NeptuneState a }
    deriving (Functor, Applicative, Monad)

{-| Data structure for configuring an application. -}
data NeptuneState = NS { nDomain :: Domain
                       , nHandlers :: [Handler]
                       , nReversers :: Map EndpointId Reverse
                       , nErrorHandlers :: ErrorHandlers
                       , nConfig :: Vault
                       }

{-| \"Compile\" the Neptune monad down to a simple configuration. -}
buildNeptune :: Domain -- ^ domain name to run under (honestly, just a URL prefix)
             -> Vault -- ^ additional configuration
             -> Neptune -- ^ configure the application
             -> NeptuneState -- ^ this is then used to serve any number of requests
buildNeptune domain config = flip execState zero . unNeptune 
    where
    zero = NS { nDomain = domain
              , nHandlers = []
              , nReversers = Map.empty
              , nErrorHandlers = def
              , nConfig = config
              }

buildSubNeptune :: Vault -> Neptune -> NeptuneState
buildSubNeptune config = buildNeptune undefined config


{-| Request handling can make it through the pipeline smoothly, or it might exit the
    pipeline prematurely. The 'Result' data type handles this. It is essentially
    the 'Error' monad, but without the wacky restrictions, and specialized to
    Neptune's purposes.
-}
data Result a = Normal a
              | Alternate Response

instance Functor Result where
    fmap = liftM
instance Applicative Result where
    pure = return
    (<*>) = ap
instance Alternative Result where
    empty = Alternate (error "empty Result")
    (Normal x) <|> _ = Normal x
    _ <|> y = y
instance Monad Result where
    return = Normal
    Normal x >>= k = k x
    Alternate x >>= k = Alternate x

{-| The 'Result' monad usually needs to be combined with other computations, esp. 'IO'. -}
newtype ResultT m a = ResultT { unResultT :: m (Result a) }
{-| Get a result in another monad. -}
runResultT :: (Monad m) => ResultT m a -> m (Result a)
runResultT = unResultT

instance Monad m => Functor (ResultT m) where fmap = liftM
instance Monad m => Applicative (ResultT m) where
    pure = return
    (<*>) = ap
instance Monad m => Alternative (ResultT m) where
    empty = ResultT $ return empty
    x <|> y = ResultT $ do
        xRes <- runResultT x
        case xRes of
            Normal x -> return $ Normal x
            _ -> runResultT y
instance Monad m => Monad (ResultT m) where
    return = ResultT . return . Normal
    x >>= k = ResultT $ unResultT x >>= \x'wrap -> case x'wrap of
        Normal x' -> unResultT . k $ x'
        Alternate x' -> return (Alternate x')
instance MonadTrans ResultT where lift = ResultT . liftM Normal
instance MonadIO m => MonadIO (ResultT m) where liftIO = lift . liftIO

{-| The 'Result' monad crops up throughout the process of handling a request.
    Since the request handling process uses several sub-monadic computations, 
    we provide this class for convenience.
-}
class Monad m => ResultMonad m where
    raise :: Response -> m a

instance Monad m => ResultMonad (ResultT m) where
    raise = ResultT . return . Alternate
instance ResultMonad RouterM where
    raise = Router . lift . lift .lift . ResultT . return . Alternate
instance ResultMonad ActionM where
    raise = Action . lift . ResultT . return . Alternate
instance ResultMonad FormatM where
    raise = Format . lift . ResultT . return . Alternate


{-
Routing takes place using a route monad, which must keep some state:
    path remaining to match
    method to match
    parameter accumulation
Reversing a url is a process of building up a PathInfo from a vault.
A Route includes formaulae to both match an incoming URL and produce an outgoing url
-}
{-| A route is a combination of forward-routing and URL-reversal.
    Since these two processes are inherently linked, this type will ease
    developer interfaces for building routes.
-}
data Route = R Router Reverse

{-| The 'RouterM' monad is primarily a consumer/producer,
    so this type should come up for complete routing computations.
-}
type Router = RouterM ()

{-| Routing involves tracking acculumated data, the path remaining, and 
    short-circuiting failing routing attempts.

    For convenience in creating suitably discoverable error responses, we
    also track what methods are allowable on the resources that could have
    been routed to.
-}
newtype RouterM a = Router { unRoute :: StateT RoutingState (MaybeT (WriterT [Verb] (ResultT IO))) a }
    deriving (Functor, Applicative, Monad, MonadIO)

{-| Perform a single route attempt. If the route fails, then we
    don't have to fully exit the 'RouterM' monad. Instead, we simply
    continue to accumulate verbs and side-effects.
-}
runRouteM :: RoutingState
          -> RouterM a
          -> WriterT [Verb] (ResultT IO) (Maybe RoutingState)
runRouteM s = runMaybeT . flip execStateT s . unRoute

{-| The is the \"second half\" of 'runRouteM'. It assumes that all routes
    are being attempted, and so may succeed with a route, or fail to find
    any match. In the case of failure, there may have been some URLs that
    matched, but the requested verb was inappropriate. If the URL did not
    match at all, then the list of verbs will be empty.
-}
runRoutesM :: WriterT [Verb] (ResultT IO) (Maybe a)
           -> ResultT IO (Either [Verb] a)
runRoutesM action = do
    (m_result, allowed) <- runWriterT action
    return $ case m_result of
        Nothing -> Left allowed
        Just result -> Right result

{-| Maintain state during a single routing attempt. -}
data RoutingState = RS { rPath :: PathInfo
                       , rData :: Vault
                       , rRequest :: Request
                       , rNeptune :: NeptuneState
                       }

{-| Since reversing a route is mainly a accumulation, this type will come up often. -}
type Reverse = ReverseM ()

{-| Reversing a route accumulates a a URL while reading from a store of parameters,
    but it may fail, particularly when necesary parameters are missing.
-}
newtype ReverseM a = Reverse { unReverse :: ReaderT ReverseState (StateT (Maybe Domain, PathInfo) Maybe) a}
    deriving (Functor, Applicative, Monad)

{-| Perform a URL reversal. -}
runReverseM :: ReverseState
            -> Reverse
            -> Maybe (Maybe Domain, PathInfo)
runReverseM s = flip execStateT (Nothing, []) . flip runReaderT s . unReverse

type ReverseState = (Vault, NeptuneState)

{-| A handler can be a single endpoint or a \"sub-application\",
    which is number of endpoints all sharing a path prefix.
    
    During routing, if an endpoint is matched, handling begins ('Action' and 'Format' computations).
    If a sub-app is reached, then it will be checked for matching before moving on to other
    possibilites.

    Sub-handlers are essential in improving routing performance in a large app.
    Without sub-apps, routes are searched in linear time.
    With appropriate resource structure and matching sub-apps, search time
    can be made logarithmic in the number of resources.
-}
data Handler = Endpoint Router Verb Action
             | Include Router [Handler]

{-| During handling, several data must be maintained for reading and possibly for writing.
    The data include the 'Request' and 'Response', and the parameter store. Additionally,
    the 'NeptuneState' is carried along to provide access to URL reversal, or anything else
    we might want to use.
-}
data HandlingState = HS { hRequest :: Request
                        , hData :: Vault
                        , hResponse :: Response
                        , hNeptune :: NeptuneState
                        }

{-| The 'ActionM' monad is primarily concerned with producing a '[(MediaType, Format)]',
    so this type will come up often. -}
type Action = ActionM [(MediaType, Format)]

{-| The 'ActionM' monad has read/write access to the 'HandlingState', but this is really
    only for updating the 'Response' therein.
-}
newtype ActionM a = Action { unAction :: StateT HandlingState (ResultT IO) a }
    deriving (Functor, Applicative, Monad, MonadIO)

{-| Run an action computation and include the updates 'HandlingState'. -}
runActionM :: HandlingState
           -> ActionM a
           -> ResultT IO (a, HandlingState)
runActionM s = flip runStateT s . unAction

{-| The 'FormatM' monad is primarily concerned with producing a 'Response' 'body',
    so this type will come up often. -}
type Format = FormatM ResponseBody

{-| The 'FormatM' monad only has read access to the 'HandlingState'.

    In contrast to other frameworks which use a crippled template language to
    enforce separation between view and controller, we enforce only that the 'Action'
    is responsible for accumulating Haskell data and response metadata. While
    the developer could write 'Format's that write to a filesystem/database/socket,
    we hope they'll have better taste than this.
-}
newtype FormatM a = Format { unFormat :: ReaderT HandlingState (ResultT IO) a }
    deriving (Functor, Applicative, Monad, MonadIO)

{-| Build a response body. -}
runFormatM :: HandlingState
           -> FormatM a
           -> ResultT IO a
runFormatM s = flip runReaderT s . unFormat

{-| Aggregates handlers for anything that is likely to go wrong.
    
    These handlers undergo content-negotiation, the same as any other
    'Response' 'body' generator. That way, human laymen don't get raw
    JSON data and automatied clients don't have to parse through HTML.
-}
data ErrorHandlers = EHs
    { ehBadContent :: [(MediaType, [MediaType] -> LByteString)]
    , ehBadResource :: [(MediaType, LByteString)]
    , ehBadVerb :: [(MediaType, [Verb] -> LByteString)]
    , ehBadAccept :: [(MediaType, [MediaType] -> LByteString)]
    , ehBadLanguage :: [(MediaType, LByteString)]
    , ehBadPermissions :: [(MediaType, LByteString)]
    , ehNoUrlReverse :: [(MediaType, EndpointId -> Vault -> LByteString)]
    , ehTimeout :: [(MediaType, DiffTime -> LByteString)]
    , ehInternalError :: [(MediaType, LByteString)]
    }
instance Default ErrorHandlers where
    def = EHs
        { ehBadContent = []
        , ehBadResource = []
        , ehBadVerb = []
        , ehBadAccept = []
        , ehBadLanguage = []
        , ehBadPermissions = []
        , ehNoUrlReverse = []
        , ehTimeout = []
        , ehInternalError = []
        }


{-| Any monad from which a 'Request' can be retrieved. -}
class Monad m => RequestMonad m where
    request :: m Request

{-| Any monad from which the vault may be accessed. -}
class Monad m => DatumMonad m where
    datum :: Key a -> m (Maybe a)

{-| Any monad in which URLs may be reversed. -}
class Monad m => ReverseMonad m where
    url :: EndpointId -> Vault -> [(Text, ByteString)] -> m Location

class Monad m => ConfigMonad m where
    config :: Key a -> m (Maybe a)


instance ConfigMonad NeptuneM where
    config key = Vault.lookup key . nConfig <$> Neptune get


{-| Attempt to route through a single handler
    (which may be a single endpoint, or a sub-app).
-}
evalHandler :: RoutingState
            -> Handler
            -> WriterT [Verb] (ResultT IO) (Maybe (Vault, Action))
evalHandler s (Endpoint route method action) = do
    result <- runRouteM s route
    case result of
        Nothing -> return Nothing
        Just result ->
            if null (rPath result)
                then if method == verb (rRequest result)
                        then return $ Just (rData result, action)
                        else const Nothing <$> tell [method]
                else return Nothing
evalHandler s (Include route subhandlers) = do
    result <- runRouteM s route
    case result of
        Nothing -> return Nothing
        Just s' -> evalHandlers s' subhandlers

{-| Attempt to route through any of a list of handlers. -}
evalHandlers :: RoutingState
             -> [Handler]
             -> WriterT [Verb] (ResultT IO) (Maybe (Vault, Action))
evalHandlers s [] = return Nothing
evalHandlers s (h:hs) = do
    m_result <- evalHandler s h
    case m_result of
        Nothing -> evalHandlers s hs
        Just result -> return $ Just result

negotiate :: AcceptMedia -> [(MediaType, a)] -> Maybe (MediaType, a)
negotiate accept formats = Wai.mapAccept (map f formats) accept
    where
    f (a, b) = (a, (a, b))

reverseUrl :: NeptuneState -> EndpointId -> Vault -> [(Text, ByteString)] -> Maybe Location
--FIXME also reverse the query string
reverseUrl s eid args query = do
    endpoint <- eid `Map.lookup` (nReversers s)
    (m_domain, path) <- runReverseM (args, s) endpoint
    let domain = fromMaybe (nDomain s) m_domain
    return (domain, path, Map.fromList query)




