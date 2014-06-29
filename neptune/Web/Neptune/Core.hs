{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, RankNTypes #-}
module Web.Neptune.Core where

import Web.Neptune.Util

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









--FIXME these need to be abstracted over
type EndpointId = Text
type Domain = Text
type PathInfo = [Text]
type Location = (Maybe Domain, PathInfo, Map Text ByteString)
type Method = ByteString
type MediaType = Wai.MediaType
type Language = Text --FIXME
type Expiry = Integer --number of seconds into the future
type AppState = ByteString
type Attachment = Wai.FileInfo LByteString
type AcceptMedia = [Wai.Quality MediaType]
type AcceptLang = [Wai.Quality Language]

data Request = Request
    { location :: PathInfo
    , verb :: Method
    , acceptType :: AcceptMedia
    , acceptLang :: AcceptLang
    , appState :: Map Text AppState
    , parameters :: Map Text [ByteString]
    , attachments :: Map Text [Attachment]
    , reqBody :: Maybe (MediaType, LByteString)
    }
data Response = Response
    { mimetype :: Maybe MediaType
    , language :: Maybe Language
    , cacheFor :: Maybe Expiry
    , updateAppState :: Map Text (Maybe (AppState, Maybe Expiry))
    , body :: LByteString --FIXME more options for things to return
    }
              | EmptyResponse Response Text --the Text is like an error code
              | Redirect      Location Bool --the Bool means it is permanent
              | BadContent    [MediaType] -- the types the app can consume
              | BadResource    
              | BadMethod     [Method]
              | BadAccept     [MediaType] -- the types the app can produce
              | BadLanguage    
              | NotAuthorized  
              | NoUrlReverse  EndpointId Vault
              | Timeout        --TODO time taken
              | InternalError  
              --TODO? a Debug response

instance Default Response where
    def = Response
        { mimetype = Nothing
        , language = Nothing
        , cacheFor = Nothing
        , updateAppState = Map.empty
        , body = ""
        }







{-
The neptune monad is responsible for accumulating routes.
The result of building a neptune can handle many requests.
Essentially, it is a configuration monad, and so has no appreciable dynamics.
-}
newtype NeptuneM a = Neptune { unNeptune :: State NeptuneState a }
    deriving (Functor, Applicative, Monad)
data NeptuneState = NS { nHandlers :: [Handler]
                       , nReversers :: Map EndpointId Reverse
                       , nDomain :: Domain
                       , nErrorHandlers :: ErrorHandlers
                       } --TODO error formatters
type Neptune = NeptuneM ()
buildNeptune :: Domain
             -> Neptune
             -> NeptuneState
buildNeptune domain = flip execState zero . unNeptune 
    where
    zero = NS { nHandlers = []
              , nReversers = Map.empty
              , nDomain = domain
              , nErrorHandlers = def
              }


{-
The entire process of servicing a request takes place in a Result monad,
which essentially a specific error monad.

The process includes routing, action and formatting, which are strictly distinguished
by the type system. The application developer may escape the pipeline at any point
for a variety of reasons using this monad.
-}
data Result a = Normal a
              | Alternate Response

class Monad m => ResultMonad m where
    raise :: Response -> m a
class Monad m => RequestMonad m where
    request :: m Request
    requests :: (Request -> a) -> m a
    requests f = liftM f request
class Monad m => ParamMonad m where
    param :: Key a -> m (Maybe a)
class Monad m => ReverseMonad m where
    url :: EndpointId -> Vault -> [(Text, ByteString)] -> m Location


{-
Handling includes everything after a successful route match.
-}

{-
Routing takes place using a route monad, which must keep some state:
    path remaining to match
    method to match
    parameter accumulation
Reversing a url is a process of building up a PathInfo from a vault.
A Route includes formaulae to both match an incoming URL and produce an outgoing url
-}
data Route = R Router Reverse

newtype RouterM a = Router { unRoute :: StateT RoutingState (MaybeT (WriterT [Method] (ResultT IO))) a }
    deriving (Functor, Applicative, Monad, MonadIO)
data RoutingState = RS { rPath :: PathInfo
                       , rParams :: Vault
                       , rRequest :: Request
                       }
type Router = RouterM ()
runRouteM :: RoutingState
          -> RouterM a
          -> WriterT [Method] (ResultT IO) (Maybe RoutingState)
runRouteM s = runMaybeT . flip execStateT s . unRoute
runRoutesM :: WriterT [Method] (ResultT IO) (Maybe a)
           -> ResultT IO (Either [Method] a)
runRoutesM action = do
    (m_result, allowed) <- runWriterT action
    return $ case m_result of
        Nothing -> Left allowed
        Just result -> Right result

newtype ReverseM a = Reverse { unReverse :: ReaderT Vault (StateT (Maybe Domain, PathInfo) Maybe) a}
    deriving (Functor, Applicative, Monad)
type Reverse = ReverseM ()
runReverseM :: Vault
            -> Reverse
            -> Maybe (Maybe Domain, PathInfo)
runReverseM s = flip execStateT (Nothing, []) . flip runReaderT s . unReverse

{-
A handler matches routes to handling actions.
The handling process includes everything after the successful route match.
-}
data Handler = Endpoint Router Method Action
             | Include Router [Handler]
data HandlingState = HS { hRequest :: Request
                        , hParams :: Vault
                        , hResponse :: Response
                        , hNeptune :: NeptuneState
                        }

{-
Handling is further divided into action and formatting.
Where actions have read/write access, formatting only has read access to the handling state.
-}
newtype ActionM a = Action { unAction :: StateT HandlingState (ResultT IO) a }
    deriving (Functor, Applicative, Monad, MonadIO)
type Action = ActionM [(MediaType, Format)]
runActionM :: HandlingState
           -> ActionM a
           -> ResultT IO (a, HandlingState)
runActionM s = flip runStateT s . unAction

newtype FormatM a = Format { unFormat :: ReaderT HandlingState (ResultT IO) a }
    deriving (Functor, Applicative, Monad, MonadIO)
type Format = FormatM LByteString
runFormatM :: HandlingState
           -> FormatM a
           -> ResultT IO a
runFormatM s = flip runReaderT s . unFormat


data ErrorHandlers = EHs
    { ehBadContent :: [(MediaType, [MediaType] -> Format)]
    , ehBadResource :: [(MediaType, Format)]
    , ehBadMethod :: [(MediaType, [Method] -> Format)]
    , ehBadAccept :: [(MediaType, [MediaType] -> Format)]
    , ehBadLanguage :: [(MediaType, Format)]
    , ehNotAuthorized :: [(MediaType, Format)]
    , ehNoUrlReverse :: [(MediaType, EndpointId -> Vault -> Format)]
    , ehTimeout :: [(MediaType, Format)]
    , ehInternalError :: [(MediaType, Format)]
    }
instance Default ErrorHandlers where
    def = EHs
        { ehBadContent = []
        , ehBadResource = []
        , ehBadMethod = []
        , ehBadAccept = []
        , ehBadLanguage = []
        , ehNotAuthorized = []
        , ehNoUrlReverse = []
        , ehTimeout = []
        , ehInternalError = []
        }


{- These functions are here to help dispatch processes. -}
evalHandler :: RoutingState
            -> Handler
            -> WriterT [Method] (ResultT IO) (Maybe (Vault, Action))
evalHandler s (Endpoint route method action) = do
    result <- runRouteM s route
    case result of
        Nothing -> return Nothing
        Just result ->
            if null (rPath result) && method == verb (rRequest result)
                then return $ Just (rParams result, action)
                else const Nothing <$> tell [method]
evalHandler s (Include route subhandlers) = do
    result <- runRouteM s route
    case result of
        Nothing -> return Nothing
        Just s' -> evalHandlers s' subhandlers
evalHandlers :: RoutingState
             -> [Handler]
             -> WriterT [Method] (ResultT IO) (Maybe (Vault, Action))
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
    (domain, path) <- runReverseM args endpoint
    return (domain, path, Map.fromList query)


{- Below are instances for the result monad and monad transformer. -}
instance Functor Result where
    fmap = liftM
instance Applicative Result where
    pure = return
    (<*>) = ap
instance Monad Result where
    return = Normal
    Normal x >>= k = k x
    Alternate x >>= k = Alternate x

newtype ResultT m a = ResultT { unResultT :: m (Result a) }
runResultT :: (Monad m) => ResultT m a -> m (Result a)
runResultT = unResultT
instance Monad m => Functor (ResultT m) where
    fmap = liftM
instance Monad m => Applicative (ResultT m) where
    pure = return
    (<*>) = ap
instance Monad m => Monad (ResultT m) where
    return = ResultT . return . Normal
    x >>= k = ResultT $ unResultT x >>= \x'wrap -> case x'wrap of
        Normal x' -> unResultT . k $ x'
        Alternate x' -> return (Alternate x')
instance MonadTrans ResultT where
    lift = ResultT . liftM Normal
instance MonadIO m => MonadIO (ResultT m) where
    liftIO = lift . liftIO

instance Monad m => ResultMonad (ResultT m) where
    raise = ResultT . return . Alternate
instance ResultMonad RouterM where
    raise = Router . lift . lift .lift . ResultT . return . Alternate
instance ResultMonad ActionM where
    raise = Action . lift . ResultT . return . Alternate
instance ResultMonad FormatM where
    raise = Format . lift . ResultT . return . Alternate

