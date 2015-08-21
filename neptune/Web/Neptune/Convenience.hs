module Web.Neptune.Convenience (
    -- * Convenience Monad Classes
      RequestMonad(..)
    , requests
    , queryAll
    , query
    , attachment
    , i12izer
    , DatumMonad(..)
    , datumOr
    , datum_f
    , ReverseMonad(..)
    , ConfigMonad(..)
    ) where

import Web.Neptune.Core
import Web.Neptune.Escape
import qualified Network.HTTP.Media as Web

import qualified Data.Map as Map
import qualified Data.Vault.Lazy as Vault

import Control.Monad.Reader
import Control.Monad.State


{-| Any monad from which a 'Request' can be retrieved. -}
class Monad m => RequestMonad m where
    request :: m Request

-- |Obtain the 'Request' and extract some more relevant data from it.
requests :: (RequestMonad m) => (Request -> a) -> m a
requests f = f `liftM` request

-- |Obtain all query parameters under the given parameter name.
queryAll :: (RequestMonad m) => Text -> m [Parameter]
queryAll key = (fromMaybe [] . Map.lookup key) `liftM` requests queries

-- |Obtain the first query parameter under the given parameter name.
query :: (RequestMonad m) => Text -> m (Maybe Parameter)
query key = do
    res <- queryAll key
    return $ case res of
        [] -> Nothing
        (x:_) -> Just x

-- |Obtain the attachments under the given name.
attachment :: (RequestMonad m) => Text -> m [Attachment]
attachment key = (fromMaybe [] . Map.lookup key) `liftM` requests attachments

{-| Given a default language and a list of server-side available languages,
    perform language negotiation.
    The result can be feed language-dependent values and the correct language
    will be retrieved.
-}
i12izer :: (RequestMonad m) => Language -> [Language] -> m ((Language -> a) -> a)
i12izer def server = do
    lang <- (fromMaybe def . Web.matchQuality server) `liftM` requests acceptLang
    return ($ lang)

{-| Any monad from which the vault may be accessed. -}
class Monad m => DatumMonad m where
    datum :: Key a -> m (Maybe a)


-- |Obtain a datum, but use the passed value when the datum does not exist.
datumOr :: (DatumMonad m) => a -> Key a -> m a
datumOr whenMissing key = fromMaybe whenMissing `liftM` datum key

-- |Obtain a datum, but raise an 'InternalError' when the datum does not exist.
datum_f :: (DatumMonad m, ResultMonad m) => Key a -> m a
datum_f key = do
    m_x <- datum key
    case m_x of
        Nothing -> raise $ InternalError "No such datum."
        Just x -> return x

{-| Any monad in which URLs may be reversed. -}
class Monad m => ReverseMonad m where
    url :: EndpointId -> Vault -> [(Text, ByteString)] -> m URL

{-| Any monad in which the server configuration may be obtained. -}
class Monad m => ConfigMonad m where
    config :: Key a -> m (Maybe a)


instance ConfigMonad NeptuneM where
    config key = Vault.lookup key . nlConfig <$> Neptune get


instance RequestMonad RouterM where
    request = rRequest <$> Router get

instance DatumMonad RouterM where
    datum key = Router $ do
        vault <- rData <$> get
        return $ key `Vault.lookup` vault

instance ConfigMonad RouterM where
    config key = Vault.lookup key . nConfig . rNeptune <$> Router get


instance DatumMonad ReverseM where
    datum key = Reverse $ Vault.lookup key <$> asks fst

instance ConfigMonad ReverseM where
    config key = Reverse $ asks (Vault.lookup key . nConfig . snd)


instance RequestMonad ActionM where
    request = hRequest <$> Action get

instance DatumMonad ActionM where
    datum key = Action $ do
        vault <- hData <$> get
        return $ key `Vault.lookup` vault

instance ConfigMonad ActionM where
    config key = Vault.lookup key . nConfig . hNeptune <$> Action get

instance ReverseMonad ActionM where
    url eid args queryParams = do
        s <- hNeptune <$> Action get
        case reverseUrl s eid args queryParams of
            Nothing -> internalError $ "Error: could not reverse url " <> eid
            Just res -> return res


instance RequestMonad FormatM where
    request = Format $ asks hRequest

instance DatumMonad FormatM where
    datum key = Format $ do
        vault <- asks hData
        return $ key `Vault.lookup` vault

instance ConfigMonad FormatM where
    config key = Format $ asks $ Vault.lookup key . nConfig . hNeptune

instance ReverseMonad FormatM where
    url eid args queryParams = do
        s <- Format $ asks hNeptune
        case reverseUrl s eid args queryParams of
            Nothing -> internalError $ "Error: could not reverse url " <> eid
            Just res -> return res
