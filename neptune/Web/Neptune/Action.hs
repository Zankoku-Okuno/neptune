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
import Web.Neptune.Escape

import qualified Data.Map as Map
import qualified Data.Vault.Lazy as Vault
import Control.Monad.State

instance DatumMonad ActionM where
    datum key = Action $ do
        vault <- hData <$> get
        return $ key `Vault.lookup` vault

parseBody :: [(MediaType, LByteString -> ActionM a)] -> ActionM a
parseBody parsers = do
    m_body <- requestBody . hRequest <$> Action get
    (mimetype, content) <- case m_body of
        Nothing -> raise $ BadContent $ fst <$> parsers
        Just x -> return x
    case mimetype `lookup` parsers of
        Nothing -> raise $ BadContent $ fst <$> parsers
        Just parser -> parser content

setCache :: Expiry -> ActionM ()
setCache seconds = Action $ modify $ \s -> s
    { hResponse = (hResponse s) {cacheFor = Just seconds} }

noCache :: ActionM ()
noCache = Action $ modify $ \s -> s
    { hResponse = (hResponse s) {cacheFor = Nothing} }

setAppState :: Text -> Maybe Expiry -> AppState -> ActionM ()
setAppState key m_expires value = Action $ do
    stateTransfers <- updateAppState . hResponse <$> get
    let stateTransfers' = Map.insert key (Just (value, m_expires)) stateTransfers
    modify $ \s -> s { hResponse = (hResponse s) {updateAppState = stateTransfers'} }

delAppState :: Text -> ActionM ()
delAppState key = Action $ do
    stateTransfers <- updateAppState . hResponse <$> get
    let stateTransfers' = Map.insert key Nothing stateTransfers
    modify $ \s -> s { hResponse = (hResponse s) {updateAppState = stateTransfers'} }

instance ReverseMonad ActionM where
    url eid args query = do
        s <- hNeptune <$> Action get
        case reverseUrl s eid args query of
            Nothing -> internalError $ "Error: could not reverse url " <> eid
            Just res -> return res

instance RequestMonad ActionM where
    request = hRequest <$> Action get

instance ConfigMonad ActionM where
    config key = Vault.lookup key . nConfig . hNeptune <$> Action get


newtype FormatsM a = Formats { unFormats :: State [(MediaType, Format)] a }
    deriving(Functor, Applicative, Monad)
type Formats = FormatsM ()

format :: Formats -> Action
format = return . flip execState [] . unFormats

medium :: MediaType -> Format -> Formats
medium mt f = Formats $ modify (++ [(mt, f)])

anyFormat :: Format -> Action
anyFormat = format . medium "*/*"