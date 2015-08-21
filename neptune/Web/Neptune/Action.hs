module Web.Neptune.Action (
      Action, ActionM
    , DatumMonad(datum)
    , parseBody
    , setCache, noCache
    , setAppState, delAppState
    , ReverseMonad(url)
    , RequestMonad(request)
    , ConfigMonad(config)

    , Formats, FormatsM
    , format
    , medium
    , anyFormat
    ) where

import Web.Neptune.Core
import Web.Neptune.Convenience

import qualified Data.Map as Map
import Control.Monad.State


{-| Extract the body of a request by performing parsing.
    The exact parser used is determined by examining the content-type
    set by the client. If no parser is available for the give
    content-type, then raise a BadContent error.
-}
parseBody :: [(MediaType, LByteString -> ActionM a)] -> ActionM a
parseBody parsers = do
    m_body <- requestBody . hRequest <$> Action get
    (mimetype, content) <- case m_body of
        Nothing -> raise $ BadContent $ fst <$> parsers
        Just x -> return x
    case mimetype `lookup` parsers of
        Nothing -> raise $ BadContent $ fst <$> parsers
        Just parser -> parser content

-- |Instruct proxies to cache this response for a number of seconds.
setCache :: Expiry -> ActionM ()
setCache seconds = Action $ modify $ \s -> s
    { hResponse = (hResponse s) {cacheFor = Just seconds} }

-- |Instruct proxies not to cache this response.
noCache :: ActionM ()
noCache = Action $ modify $ \s -> s
    { hResponse = (hResponse s) {cacheFor = Nothing} }

-- |Instruct the client to create/update data in its local store.
setAppState :: Text -> Maybe Expiry -> AppState -> ActionM ()
setAppState key m_expires value = Action $ do
    stateTransfers <- updateAppState . hResponse <$> get
    let stateTransfers' = Map.insert key (Just (value, m_expires)) stateTransfers
    modify $ \s -> s { hResponse = (hResponse s) {updateAppState = stateTransfers'} }

-- |Instruct the client to remove data from its local store.
delAppState :: Text -> ActionM ()
delAppState key = Action $ do
    stateTransfers <- updateAppState . hResponse <$> get
    let stateTransfers' = Map.insert key Nothing stateTransfers
    modify $ \s -> s { hResponse = (hResponse s) {updateAppState = stateTransfers'} }


-- |A simple accumulation monad for gathering representations.
newtype FormatsM a = Formats { unFormats :: State [(MediaType, Format)] a }
    deriving(Functor, Applicative, Monad)
-- |See 'FormatsM'.
type Formats = FormatsM ()

-- |Run a 'Formats' monad to finish off an 'Action'.
format :: Formats -> Action
format = return . flip execState [] . unFormats

-- |Add a 'MediaType'-'Format' pair to the possible representations.
medium :: MediaType -> Format -> Formats
medium mt f = Formats $ modify (++ [(mt, f)])

-- |Use the passed representation to repond to request regardless of the
--  client's preferred representation.
anyFormat :: Format -> Action
anyFormat = format . medium "*/*"