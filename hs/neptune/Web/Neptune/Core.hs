{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, RankNTypes #-}
module Web.Neptune.Core where

import Web.Neptune.Util

import Data.Time.Clock

import Data.String (IsString(..))
import Data.Default
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vault.Lazy (Vault, Key)
import qualified Data.Vault.Lazy as Vault
import Data.ByteString.Lazy (ByteString)

import Control.Monad.Identity
import Control.Monad.Maybe
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as Wai
import qualified Network.HTTP.Types as Wai
import qualified Network.HTTP.Media as Wai









--FIXME these need to be abstracted over
type EndpointId = Text
type PathInfo = [Text]
type Method = Wai.Method
type MediaType = Wai.MediaType
type Language = Text --FIXME
type Expiry = Integer --number of seconds into the future
type AppState = ByteString
type Attachment = Wai.FileInfo ByteString
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
    , reqBody :: ByteString
    }
data Response = Response
    { mimetype :: Maybe MediaType
    , language :: Maybe Language
    , cacheFor :: Maybe Expiry
    , updateAppState :: Map Text (Maybe (AppState, Maybe Expiry))
    , body :: ByteString --FIXME more options for things to return
    }
              | EmptyResponse Response Text --the Text is like an error code
              | Redirect      PathInfo Bool --the Bool means it is permanent
              | BadResource
              | BadMethod     [Method]
              | BadMimetype   --(!use the Accept-Ranges response header!)
              | BadLanguage
              | NotAuthorized
              | NoUrlReverse  EndpointId Vault
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
newtype NeptuneM a = NeptuneM { unNeptune :: State NeptuneState a }
    deriving (Functor, Applicative, Monad)
data NeptuneState = NS { nHandlers :: [Handler]
                       , nReversers :: Map EndpointId Reverse
                       } --TODO error formatters
type Neptune = NeptuneM ()
buildNeptune :: Neptune
             -> NeptuneState
buildNeptune = flip execState zero . unNeptune 
    where
    zero = NS { nHandlers = []
              , nReversers = Map.empty
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
    url :: EndpointId -> Vault -> m PathInfo


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

newtype ReverseM a = Reverse { unReverse :: ReaderT Vault (WriterT [Text] Maybe) a}
    deriving (Functor, Applicative, Monad)
type Reverse = ReverseM ()
runReverseM :: Vault
            -> Reverse
            -> Maybe [Text]
runReverseM s = execWriterT . flip runReaderT s . unReverse

{-
A handler matches routes to handling actions.
The handling process includes everything after the successful route match.
-}
data Handler = Endpoint Router Method Action
             | Include Router [Handler]
data HandlingState = HS { hRequest :: Request
                        , hParams :: Vault
                        , hResponse :: Response
                        , hReversers :: Map EndpointId Reverse
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
type Format = FormatM ByteString
runFormatM :: HandlingState
           -> FormatM a
           -> ResultT IO a
runFormatM s = flip runReaderT s . unFormat


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

negotiate :: Request -> [(MediaType, (MediaType, a))] -> Maybe (MediaType, a)
negotiate request formats = Wai.mapAccept formats (acceptType request)

reverseUrl :: HandlingState -> EndpointId -> Vault -> Maybe PathInfo --FIXME also reverse the query string
reverseUrl s eid args = runReverseM args =<< eid `Map.lookup` hReversers s


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















waiToNeptune :: Wai.Request -> Request
waiToNeptune r = Request
    { location = Wai.pathInfo r
    , verb = Wai.requestMethod r
    , acceptType = acceptType
    , acceptLang = error "toNeptune: get acceptLang" --STUB
    , appState = error "toNeptune: get appState" --STUB
    , parameters = error "toNeptune: get parameters" --STUB
    , attachments = error "toNeptune: get attachments" --STUB
    , reqBody = error "toNeptune: get reqBody" --STUB
    }
    where
    headers = Wai.requestHeaders r
    acceptType = let accept = fromMaybe "*/*" $ "Accept" `lookup` headers
                 in fromMaybe [] $ Wai.parseAccept accept
waiFromNeptune :: Response -> Wai.Response
waiFromNeptune r@(Response {}) = Wai.responseLBS Wai.status200 headers (body r) --FIXME add headers
    where
    headers = mimeHeader ++ langHeader ++ cacheHeader ++ cookies
    mimeHeader = case mimetype r of
        Nothing -> []
        Just mt -> [("Content-Type", fromString . show $ mt)]
    langHeader = [] --STUB
    cacheHeader = case cacheFor r of
        Nothing -> [ ("Cache-Control", "private, max-age=0, no-cache, no-store")]
        Just dt -> [ ("Cache-Control", "no-transform, public, max-age=" <> (fromString . show) dt)
                   , ("Vary", "Accept,Accept-Language,Accept-Encoding") ] --TODO check that this is all varying needed
    cookies = [] --STUB
waiFromNeptune _ = error "waiFromNeptune: alternate responses" --STUB


serveWai :: Neptune -> Wai.Application
serveWai neptune = app --TODO make sure exceptions get turned into http500
    where
    builtNeptune = buildNeptune neptune
    app waiRequest respond = (respond =<<) . handleResult $ do
        let request = waiToNeptune waiRequest
            routingState = RS { rRequest = request
                              , rPath = location request
                              , rParams = Wai.vault waiRequest
                              }
        m_route <- runRoutesM $ evalHandlers routingState (nHandlers builtNeptune)
        (vault, action) <- case m_route of
            Left [] -> raise BadResource
            Left allowed -> raise $ BadMethod allowed
            Right route -> return route
        let handlingState = HS { hRequest = request
                               , hParams = vault
                               , hResponse = def
                               , hReversers = nReversers builtNeptune
                               }
        (pre_formats, state) <- runActionM handlingState action
        let formats = (\(mt, f) -> (mt, (mt, f))) <$> pre_formats
        (mimetype, format) <- maybe (raise BadMimetype) return $ negotiate request formats
        let state' = state { hResponse = (hResponse state) {mimetype = Just mimetype} }
        body <- runFormatM state' format
        return . waiFromNeptune $ (hResponse state') { body = body }
    handleResult :: ResultT IO Wai.Response -> IO Wai.Response
    handleResult x = runResultT x >>= \res -> case res of
        Normal x' -> return x'
        Alternate response -> return $ waiFromNeptune response


