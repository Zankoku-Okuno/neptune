module Web.Neptune.Action (
      VaultMonad(vault)
    , parseBody
    , setCache, noCache
    , setAppState, delAppState
    , ReverseMonad(url)
    , RequestMonad(request, requests)
    , ConfigMonad(config)
    ) where

import Web.Neptune.Util
import Web.Neptune.Core

import qualified Data.Map as Map
import qualified Data.Vault.Lazy as Vault
import Control.Monad.State

instance VaultMonad ActionM where
    vault key = Action $ do
        vault <- hVault <$> get
        return $ key `Vault.lookup` vault

parseBody :: [(MediaType, LByteString -> ActionM a)] -> ActionM a
parseBody parsers = do
    m_body <- reqBody . hRequest <$> Action get
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
            Nothing -> raise $ NoUrlReverse eid args
            Just res -> return res

instance RequestMonad ActionM where
    request = hRequest <$> Action get

instance ConfigMonad ActionM where
    config key = Vault.lookup key . nConfig . hNeptune <$> Action get
