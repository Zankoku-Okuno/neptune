{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving, RankNTypes #-}
module Web.Neptune.Core where

import Web.Neptune.Util

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Vault.Lazy (Vault)
import qualified Data.Vault.Lazy as Vault

import Control.Monad.Identity
import Control.Monad.Maybe
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as Wai
import qualified Network.HTTP.Media as Wai









--FIXME these need to be abstracted over
type EndpointId = Text
type Request = Wai.Request
type PathInfo = [Text]
type Method = Wai.Method
type MediaType = Wai.MediaType
type Response = (Wai.Status, Wai.ResponseHeaders)









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
              | Error Wai.Status
              | WaiResult Wai.Response --only to be used internally or by extenders


{-
Handling includes everything after a successful route match.
-}

{-
Routing takes place using a route monad, which must keep some state:
	path remaining to match
	method to match
	parameter accumulation
Reversing a url is a process of building up a PathInfo from a vault
-}
data Route = R Router Reverse

newtype RouterM a = Router { unRoute :: StateT RoutingState (MaybeT (ResultT IO)) a }
    deriving (Functor, Applicative, Monad, MonadIO)
data RoutingState = RS { rPath :: PathInfo
                       , rMethod :: Method
                       , rParams :: Vault
                       , rRequest :: Request
                       }
type Router = RouterM ()
runRouteM :: RoutingState
          -> RouterM a
          -> ResultT IO (Maybe RoutingState)
runRouteM s = runMaybeT . flip execStateT s . unRoute

newtype ReverseM a = Reverse { unReverse :: ReaderT Vault (WriterT [Text] Maybe) a}
	deriving (Functor, Applicative, Monad)
type Reverse = ReverseM ()
runReverseM :: Vault
            -> ReverseM a
            -> Maybe [Text]
runReverseM s = execWriterT . flip runReaderT s . unReverse

{-
A handler matches routes to handling actions.
The handling process includes everything after the successful route match.
-}
data Handler = Endpoint Router Method Action
             | Include Router [Handler]
data HandlingState = HS { aRequest :: Request
                        , aParams :: Vault
                        , aResponse :: Response
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
type Format = FormatM Wai.Response
runFormatM :: HandlingState
           -> FormatM a
           -> ResultT IO a
runFormatM s = flip runReaderT s . unFormat


{- These functions are here to help dispatch processes. -}
evalHandler :: RoutingState
            -> Handler
            -> ResultT IO (Maybe (Vault, Action))
evalHandler s (Endpoint route method action) = do
    result <- runRouteM s route
    case result of
        Nothing -> return Nothing
        Just result ->
            if null (rPath result) && method == rMethod result
                then return $ Just (rParams result, action)
                else return Nothing
evalHandler s (Include route subhandlers) = do
    result <- runRouteM s route
    case result of
        Nothing -> return Nothing
        Just s' -> evalHandlers s' subhandlers
evalHandlers :: RoutingState
             -> [Handler]
             -> ResultT IO (Maybe (Vault, Action))
evalHandlers s [] = return Nothing
evalHandlers s (h:hs) = do
    m_result <- evalHandler s h
    case m_result of
        Nothing -> evalHandlers s hs
        Just result -> return $ Just result

negotiate :: Request -> [(MediaType, a)] -> Maybe a
negotiate request formats = Wai.parseAccept acceptText >>= Wai.mapAccept formats
    where
    acceptText = fromMaybe "*/*" . lookup "Accept" $ Wai.requestHeaders request


{- Below are instances for the result monad and monad transformer. -}
instance Functor Result where
    fmap = liftM
instance Applicative Result where
    pure = return
    (<*>) = ap
instance Monad Result where
    return = Normal
    Normal x >>= k = k x
    Error x >>= k = Error x
    WaiResult x >>= k = WaiResult x

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
        Error x' -> return (Error x')
        WaiResult x' -> return (WaiResult x')
instance MonadTrans ResultT where
    lift = ResultT . liftM Normal
instance MonadIO m => MonadIO (ResultT m) where
    liftIO = lift . liftIO


















neptuneApp :: Neptune -> Wai.Application
neptuneApp neptune =
    let builtNeptune = buildNeptune neptune
        app request respond = (respond =<<) . handleResult $ do
            (vault, action) <- fromMaybeM (raiseError Wai.status404) $
                            evalHandlers (mkRoutingState request) (nHandlers builtNeptune)
            (formats, state) <- runActionM (mkActionState request vault) action
            format <- maybe (raiseError Wai.status406) return $ negotiate request formats
            runFormatM state format
    in app --TODO make sure exceptions get turned into http500
    where
    handleResult :: ResultT IO Wai.Response -> IO Wai.Response
    handleResult x = runResultT x >>= \res -> case res of
        Normal x' -> return x'
        Error status -> undefined --STUB
        WaiResult x' -> return x'

mkRoutingState :: Request -> RoutingState
mkRoutingState request = RS { rRequest = request
                          , rMethod = Wai.requestMethod request
                          , rPath = Wai.pathInfo request
                          , rParams = Vault.empty
                          }
mkActionState :: Request -> Vault -> HandlingState
mkActionState request vault = HS { aRequest = request
                                 , aParams = vault
                                 , aResponse = (Wai.status200, [])
                                 }


raiseError :: (Monad m) => Wai.Status -> ResultT m a
raiseError = ResultT . return . Error
