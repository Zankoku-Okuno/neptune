{-#LANGUAGE OverloadedStrings, LambdaCase, RecordWildCards,
            GeneralizedNewtypeDeriving,
            ExistentialQuantification #-}
import Data.Maybe
import Data.Vault.Strict (Vault, Key)
import qualified Data.Vault.Strict as Vault
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Exception (Exception, SomeException)
import qualified Control.Exception as Exn
import Data.Typeable

import Network.HTTP.Media

import System.Directory
import System.IO.Unsafe

-- TODO implement control mechanisms (i.e. caching)
-- TODO allow custom configuration on apps (probably just the user does it with a fixpoint, and subapps requiring config are paramaterized)
-- TODO real uri type
    -- network-uri: good relitivization and normalization; uses inefficient String/parsec; no native understanding of query parameters or paths
    -- uri-bytestring: good understanding of the uri parts; too large an api?
    -- annoyingly, they all have URI instead of Uri, and no IsString instance
-- TODO implement logging with hslogger
-- TODO response bodies could be strict/lazy bytestrings, loaded from files, forwarded to other scripts
-- TODO state transfer (cookies &c)

-- TODO consider not using Vault for info
    -- a type parameter I think infects the entire system
    -- in any case, a type parameter would probably want to be typeclassed so that independent subapp developers can glue each other's code together: what a pain    -- perhaps something with extensible records would do, but Haskell doesn't make that easy


infoKey :: Key String
infoKey = unsafePerformIO $ Vault.newKey
mkInfo x = Vault.insert infoKey x Vault.empty

main = do
    let neptune = NeptuneApp
            { appInfo = mkInfo "It's-a me, testapp!"
            , endpoints =
                [ Endpoint { endpointInfo = mkInfo "echo"
                           , match = \(name, body) -> if name == "echo" then Just (pure $ fromMaybe "" body) else Nothing
                           , handler = const $ pure . ("Simon says: " ++)
                           , mkResource = flip const
                           , formats = [("text/plaintext", pure . ResponseData)]
                           }
                , Endpoint { endpointInfo = mkInfo "const"
                           , match = \(name, _) -> if name == "const" then Just (pure ()) else Nothing
                           , handler = const $ const $ pure "Valleys of Neptune is risin'..."
                           , mkResource = flip const
                           , formats = [("text/plaintext", pure . ResponseData)]
                           }
                , Endpoint { endpointInfo = mkInfo "where"
                           , match = \(name, _) -> if name == "pwd" then Just (liftIO getCurrentDirectory) else Nothing
                           , handler = const $ pure . show
                           , mkResource = flip const
                           , formats = [("text/plaintext", pure . ResponseData)]
                           }
                , Endpoint { endpointInfo = mkInfo "error"
                           , match = \(name, _) -> if name == "error" then Just (pure ()) else Nothing
                           , handler = const $ const $ error "this is the song that never ends / it just goes on and on my friends..."
                           , mkResource = flip const
                           , formats = [("text/plaintext", pure . ResponseData)]
                           }
                ]
            , customError = [("*/*", \err -> pure $ ResponseData $
                                    "There was an error processing your request.\n" ++ show err)
                            ]
            , logger = ()
            }
        app = createApp neptune
        docs = Vault.lookup infoKey <$> info neptune
        request = Request
            { uri = ("pwd", Just "Goodbyte, and good luck.")
            , verb = "GET"
            , accept = ContentPreference
                { acceptMedia = fromJust $ parseQuality "*/*" }
            }
    putStrLn "Running app described by:"
    print docs
    putStrLn "Handling request:"
    response <- app request
    putStrLn $ responseBody response





type Uri = (String, Maybe String) -- TODO
type Verb = String -- TODO
type Dispatch resource = Uri -> Maybe (Retriever resource)
type Retriever resource = ReaderT NeptuneApp IO resource
type Handler resource result = Verb -> resource -> Pipeline result
type Formats result = [(MediaType, Render result)] --TODO
type Render result = result -> Reader NeptuneApp ResponseData


data NeptuneApp = NeptuneApp
    { appInfo :: Vault
    , endpoints :: [Route]
    , customError :: Formats Error
    , logger :: () -- TODO
    -- TODO
    }
type Route = RouteWith ()
data RouteWith prereq
    = forall req resource result. Endpoint
        { endpointInfo :: Vault
        , match :: Dispatch req
        , mkResource :: prereq -> req -> resource
        , handler :: Handler resource result
        , formats :: Formats result
        }
    | forall req resource. SubApp
        { subappInfo :: Vault
        , partialMatch :: Uri -> Maybe (Retriever req, Uri)
        , mkResource :: prereq -> req -> resource
        , nextRoute :: [RouteWith resource]
        }





data ContentPreference = ContentPreference
    { acceptMedia :: [Quality MediaType]
    -- TODO language, encoding, are there others?
    }
data ContentType = ContentType
    { mediaType :: MediaType
    -- TODO language, encoding, are there others?
    }





type NeptuneServer = Request -> IO Response
data Request = Request
    { uri :: Uri
    , verb :: Verb
    , accept :: ContentPreference
    -- TODO
    }
data Serve = forall a b. Serve (Retriever a) (Handler a b) (Formats b)
data Status = Ok | Error Error
data Error
    = NoDispatch -- TODO diagnostics
    | NoAccept -- TODO diagnostics
    -- TODO
    | InternalError SomeException
    deriving (Typeable, Show)
instance Exception Error
data ResponseData = ResponseData
    { body :: String -- TODO
    -- TODO state transfer
    }
data Response = Response
    { status :: Status
    , contentType :: ContentType
    , responseBody :: String -- TODO
    -- TODO
    }

createApp :: NeptuneApp -> NeptuneServer
createApp neptune@(NeptuneApp {..}) = runPipeline neptune pipeline
    where
    pipeline = (do
        uri <- asksRequest uri
        endpoint <- endpoints `dispatch` uri
        (format, ResponseData {..}) <- process endpoint
        pure $ Response
            { status = Ok
            , contentType = format
            , responseBody = body
            }
        ) `catch` (\err -> diagnostic err <$> asksRequest accept)
    dispatch :: [Route] -> Uri -> Pipeline Serve
    rs `dispatch` uri = case goes (pure ()) uri rs of
        Nothing -> throw NoDispatch
        Just ep -> pure ep
        where
        go :: Retriever prereq -> Uri -> RouteWith prereq -> Maybe Serve
        go prereq uri (Endpoint {..}) = case match uri of
            Nothing -> Nothing
            Just req -> Just $ Serve (mkResource <$> prereq <*> req) handler formats
        go prereq uri (SubApp {..}) = case partialMatch uri of
            Nothing -> Nothing
            Just (req, uri') -> goes (mkResource <$> prereq <*> req) uri' nextRoute
        goes :: Retriever prereq -> Uri -> [RouteWith prereq] -> Maybe Serve
        goes prereq uri [] = Nothing
        goes prereq uri (r:rs) = go prereq uri r <|> goes prereq uri rs
    process :: Serve -> Pipeline (ContentType, ResponseData)
    process (Serve getResource handler formats) = do
        verb <- asksRequest verb
        resource <- liftIO $ runReaderT getResource neptune
        result <- handler verb resource
        pref <- asksRequest accept
        (format, renderer) <- case negotiate pref formats of
            Nothing -> throw NoAccept
            Just it -> pure it
        let response = result `render` renderer
        pure (format, response)
    negotiate :: ContentPreference -> Formats a -> Maybe (ContentType, Render a)
    negotiate (ContentPreference {..}) mediaOpts = do
        -- FIXME there's gotta be an efficient way to do this; perhaps I need to open an issue
        mediaType <- matchQuality (fst <$> mediaOpts) acceptMedia
        langOpts <- mapQuality mediaOpts acceptMedia
        --lang <- matchQuality (fst <$> langOpts) acceptLanguage
        --encOpts <- mapQuality langOpts acceptLanguage
        --encoding <- matchQuality (fst <$> encOpts) acceptEncoding
        v <- pure langOpts -- mapQuality encOpts acceptEncoding -- TODO
        pure (ContentType { mediaType = mediaType }, v)
    render :: result -> Render result -> ResponseData
    render result renderer = runReader (renderer result) neptune
    diagnostic :: Error -> ContentPreference -> Response
    diagnostic err pref = case negotiate pref customError of
        Nothing -> error "We don't know how to tell you this, but there was an error."
        Just (format, renderer) ->
            let ResponseData {..} = err `render` renderer
            in Response
                { status = Error err
                , contentType = format
                , responseBody = body
                }



data RoseTree a = RoseTree a [RoseTree a]
    deriving (Eq, Show)
instance Functor RoseTree where
    fmap f (RoseTree x kids) = RoseTree (f x) (fmap f <$> kids)
info :: NeptuneApp -> RoseTree Vault
info (NeptuneApp {..}) = RoseTree appInfo (go <$> endpoints)
    where
    go :: RouteWith a -> RoseTree Vault
    go (Endpoint {..}) = RoseTree endpointInfo []
    go (SubApp {..}) = RoseTree subappInfo (go <$> nextRoute)





data Result r
    = Good r
    | Bad Error
instance Functor Result where
    fmap f (Good x) = Good (f x)
    fmap _ (Bad err) = Bad err
instance Applicative Result where
    pure = Good
    (Good f) <*> (Good x) = Good (f x)
    (Good _) <*> (Bad err) = Bad err
    (Bad err) <*> _ = Bad err
instance Monad Result where
    return = pure
    (Good x) >>= f = f x
    (Bad err) >>= _ = Bad err
newtype ResultT m a = ResultT { unResultT :: m (Result a) }
instance Monad m => Functor (ResultT m) where
    fmap f (ResultT x) = ResultT $ (fmap . fmap) f x
instance Monad m => Applicative (ResultT m) where
    pure = return
    (<*>) = ap
instance Monad m => Monad (ResultT m) where
    return = ResultT . pure . pure
    (ResultT x) >>= f = ResultT $ x >>= \case
        Good r -> unResultT $ f r
        (Bad err) -> pure $ Bad err
instance MonadTrans ResultT where
    lift action = ResultT $ Good <$> action
instance (MonadIO m) => MonadIO (ResultT m) where
    liftIO action = lift $ liftIO action
liftResult :: (Monad m) => Error -> ResultT m a
liftResult x = ResultT $ pure (Bad x)

newtype Pipeline a = Pipeline { unPipeline ::
    ReaderT NeptuneApp (ReaderT Request (ResultT IO)) a
} deriving (Functor, Applicative, Monad, MonadIO)
    -- TODO logging
runPipeline :: NeptuneApp -> Pipeline a -> Request -> IO a
runPipeline app (Pipeline pipeline) =
    (wrapExn =<<) . unResultT . runReaderT (runReaderT pipeline app)
    where
    wrapExn (Good x) = pure x
    wrapExn (Bad err) = Exn.throwIO err
getApp :: Pipeline NeptuneApp
getApp = Pipeline ask
asksApp :: (NeptuneApp -> a) -> Pipeline a
asksApp f = f <$> getApp
getRequest :: Pipeline Request
getRequest = Pipeline $ lift ask
asksRequest :: (Request -> a) -> Pipeline a
asksRequest f = f <$> getRequest
throw :: Error -> Pipeline a
throw err = Pipeline $ lift . lift $ liftResult err
catch :: Pipeline a -> (Error -> Pipeline a) -> Pipeline a
catch = flip handle
handle :: (Error -> Pipeline a) -> Pipeline a -> Pipeline a
handle f (Pipeline pipeline) = do
    app <- getApp
    request <- getRequest
    let action = unResultT $ runReaderT (runReaderT pipeline app) request
    result <- liftIO $ action `Exn.catch` (pure . Bad . InternalError)
    case result of
        Good r -> pure r
        Bad err -> f err
