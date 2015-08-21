module Web.Neptune.Core (
      module Web.Neptune.Core.Util
    -- * Neptune Applications
    , Neptune
    , NeptuneM(..)
    , buildNeptune
    , execNeptune
    , NeptuneLib(..)
    , NeptuneServer(..)
    , ErrorHandlers(..)
    
    -- * Request Handling Pipeline
    -- ** Results
    , Result(..)
    , ResultT(..)
    , runResultT
    , ResultMonad(..)
    -- * Routing
    , Route(..)
    , Router
    , RouterM(..)
    , RoutingState(..)
    , runRouteM
    , runRoutesM
    , Reverse
    , ReverseM(..)
    , runReverseM
    , reverseUrl
    -- ** Handlers
    , Handler(..)
    , HandlingState(..)
    , evalHandler
    , evalHandlers
    -- ** Actions
    , Action
    , ActionM(..)
    , runActionM
    -- ** Formatting
    , Format
    , FormatM(..)
    , runFormatM
    , negotiate
    
    -- * Types
    -- ** High-level Protocol Elements
    , EndpointId
    , Request(..)
    , Response(..)
    , ResponseBody(..)
    , RedirectReason(..)
    -- ** Low-level Protocol Elements
    , URL
    , Verb
    , MediaType
    , Language
    , Web.Quality
    , AcceptMedia
    , AcceptLang
    , Parameter
    , Attachment
    , AppState
    , Expiry
    -- ** URL Types
    , Scheme
    , Host
    , PathInfo
    -- ** URL Building
    , simpleUrl
    , urlUser
    , urlPort
    , urlPath
    , urlQuery
    , urlHash
    ) where

import Web.Neptune.Core.Util
import Web.Neptune.Core.Types
import Web.Neptune.Core.Url
import qualified Network.HTTP.Media as Web

import Data.Default
import qualified Data.Map as Map

import Control.Monad.Maybe
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State


{-| Since 'NeptuneM' is essentially just an accumulator,
    this type should come up often for complete Neptune applications.
-}
type Neptune = NeptuneM ()

{-| The 'NeptuneM' monad accumulates all the application configuration.
    This includes routing, URL reversing, and error handlers.
-}
newtype NeptuneM a = Neptune { unNeptune :: State NeptuneLib a }
    deriving (Functor, Applicative, Monad)

data NeptuneLib = NL { nlHandlers :: [Handler]
                     , nlReversers :: Map EndpointId Reverse
                     , nlErrorHandlers :: ErrorHandlers
                     , nlConfig :: Vault
                     }

data NeptuneServer = NS { nPrepath :: URL
                        , nHandlers :: [Handler]
                        , nReversers :: Map EndpointId Reverse
                        , nErrorHandlers :: ErrorHandlers
                        , nConfig :: Vault
                        }

-- |Compile a Neptune sub-application.
buildNeptune :: Vault -- ^ additional configuration
             -> Neptune -- ^ configure the application
             -> NeptuneLib -- ^ this is then used to serve any number of requests
buildNeptune conf = flip execState zero . unNeptune 
    where
    zero = NL { nlHandlers = []
              , nlReversers = Map.empty
              , nlErrorHandlers = def
              , nlConfig = conf
              }

{-| \"Compile\" the Neptune monad down to a \"executable\" with resources and error handlers and so forth. -}
execNeptune :: URL -- ^ host over which the application is served
            -> NeptuneLib
            -> NeptuneServer
execNeptune prepath lib = NS { nPrepath = prepath
                             , nHandlers = nlHandlers lib
                             , nReversers = nlReversers lib
                             , nErrorHandlers = nlErrorHandlers lib
                             , nConfig = nlConfig lib
                             }


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
    Alternate x >>= _ = Alternate x

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
            (Normal _) -> return xRes
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
                       , rNeptune :: NeptuneServer
                       }

{-| Since reversing a route is mainly a accumulation, this type will come up often. -}
type Reverse = ReverseM ()

{-| Reversing a route accumulates a a URL while reading from a store of parameters,
    but it may fail, particularly when necesary parameters are missing.
-}
newtype ReverseM a = Reverse { unReverse :: ReaderT ReverseState (StateT (Maybe URL, PathInfo) Maybe) a}
    deriving (Functor, Applicative, Monad)

{-| Perform a URL reversal. -}
runReverseM :: ReverseState
            -> Reverse
            -> Maybe (Maybe URL, PathInfo)
runReverseM s = flip execStateT (Nothing, []) . flip runReaderT s . unReverse

type ReverseState = (Vault, NeptuneServer)

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
                        , hNeptune :: NeptuneServer
                        }

{-| The 'ActionM' monad is primarily concerned with producing a '[(MediaType, Format)]',
    so this type will come up often. -}
type Action = ActionM [(MediaType, Format)]

{-| The 'ActionM' monad has read access to the 'Request' and application data
    ('NeptuneServer'), as well as read/write access to the 'Response'.
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

{-| The 'FormatM' monad only has read access to the Request, Repsonse and application data
    ('NeptuneServer').

    In contrast to other frameworks which use a crippled template language to
    enforce separation between view and controller, we enforce only that the 'Action'
    is responsible for accumulating Haskell data and response metadata. While
    the developer could write 'Format's that write to a filesystem\/database\/socket,
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
    , ehTimeout :: [(MediaType, DiffTime -> LByteString)]
    , ehInternalError :: [(MediaType, Text -> LByteString)]
    }
instance Default ErrorHandlers where
    def = EHs
        { ehBadContent = []
        , ehBadResource = []
        , ehBadVerb = []
        , ehBadAccept = []
        , ehBadLanguage = []
        , ehBadPermissions = []
        , ehTimeout = []
        , ehInternalError = []
        }


{-| Attempt to route through a single handler
    (which may be a single endpoint, or a sub-app).
-}
evalHandler :: RoutingState
            -> Handler
            -> WriterT [Verb] (ResultT IO) (Maybe (Vault, Action))
evalHandler s (Endpoint route method action) = do
    m_result <- runRouteM s route
    case m_result of
        Nothing -> return Nothing
        Just result ->
            if null (rPath result)
                then if method == verb (rRequest result)
                        then return $ Just (rData result, action)
                        else Nothing <$ tell [method]
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
evalHandlers _ [] = return Nothing
evalHandlers s (h:hs) = do
    m_result <- evalHandler s h
    case m_result of
        Nothing -> evalHandlers s hs
        Just result -> return $ Just result



--FIXME negotiate and reverseUrl should integrate better with the convenience monads, RequestMonad and ReverseMonad
{-| Perform content negotiation for media types.
    If successful, return both the selected media type and its
    associated payload.
-}
negotiate :: (Web.Accept t) => [Web.Quality t] -> [(t, a)] -> Maybe (t, a)
negotiate accept formats = Web.mapQuality (map f formats) accept
    where
    f (a, b) = (a, (a, b))

{-| For internal use: perform a URL reversal.
    
    That is, find the Reverse action associated with the 'EndpointId'
    and pass is a datum 'Vault' and query parameters to retrieve a
    url.
-}
reverseUrl :: NeptuneServer -> EndpointId -> Vault -> [(Text, ByteString)] -> Maybe URL
--FIXME also reverse the query string
reverseUrl s eid args queryParams = do
    endpoint <- eid `Map.lookup` (nReversers s)
    (m_prepath, path) <- runReverseM (args, s) endpoint
    return $ fromMaybe (nPrepath s) m_prepath `urlPath` path




