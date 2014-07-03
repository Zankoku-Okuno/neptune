{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving,
             TypeSynonymInstances, FlexibleInstances #-}
module Web.Neptune.Tools (
      toStrict
    , fromStrict
    , toStrictT
    , fromStrictT

    -- TODO codecs

    , newKey

    , requests
    , queryAll
    , query

    , datumOr

    , QDatum(..)
    , qRoute
    , qDatum
    , qDatumOr
    , pathKey

    , MkVault
    , mkVault
    , (===)
    ) where

import Web.Neptune.Core
import Web.Neptune.Route

import System.IO.Unsafe
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Map as Map
import qualified Data.Vault.Lazy as Vault
import Control.Monad.State


toStrict :: LByteString -> ByteString
toStrict = LBS.toStrict
fromStrict :: ByteString -> LByteString
fromStrict = LBS.fromStrict

toStrictT :: LText -> Text
toStrictT = LT.toStrict
fromStrictT :: Text -> LText
fromStrictT = LT.fromStrict


encodeUtf8 :: Text -> ByteString
encodeUtf8 = T.encodeUtf8
--encodePercent
--encodeLatin1
encodeUtf8L :: LText -> LByteString
encodeUtf8L = LT.encodeUtf8
--encodePercentL
--encodeLatin1L

decodeUtf8 :: ByteString -> Text
decodeUtf8 = T.decodeUtf8
--decodePercent
--decodeLatin1
decodeUtf8L :: LByteString -> LText
decodeUtf8L = LT.decodeUtf8
--decodePercentL
--decodeLatin1L


newKey :: Key a
newKey = unsafePerformIO Vault.newKey


{- Request monad -}
requests :: (RequestMonad m) => (Request -> a) -> m a
requests f = f `liftM` request

queryAll :: (RequestMonad m) => Text -> m [Parameter]
queryAll key = (fromMaybe [] . Map.lookup key) `liftM` requests queries

query :: (RequestMonad m) => Text -> m (Maybe Parameter)
query key = do
    res <- queryAll key
    return $ case res of
        [] -> Nothing
        (x:_) -> Just x

attachment :: (RequestMonad m) => Text -> m [Attachment]
attachment key = (fromMaybe [] . Map.lookup key) `liftM` requests attachments

{- Datum monad -}
datumOr :: (DatumMonad m) => a -> Key a -> m a
datumOr def key = fromMaybe def `liftM` datum key
--TODO datum500 :: ResultMonad m => Key a -> m a

{- Quick Data -}
_quickKey :: Key (Map Text Text)
_quickKey = newKey

pathKey :: Key [Text]
pathKey = newKey

qRoute :: Text -> Route
qRoute name = R fore back
    where
    fore = do
        [captured] <- consume 1
        qData <- datumOr Map.empty _quickKey
        let qData' = Map.insert name captured qData
        setDatum _quickKey qData'
    back = do
        m_datum <- Map.lookup name <$> datumOr Map.empty _quickKey
        case m_datum of 
            Just datum -> create datum
            Nothing -> Reverse . lift . lift $ Nothing

qDatum :: (DatumMonad m, QDatum a) => Text -> m (Either Text a)
qDatum name = do
    m_datum <- Map.lookup name `liftM` datumOr Map.empty _quickKey
    case m_datum of
        Nothing -> return . Left $ "Quick-datum not found: '" <> name <> "'"
        Just datum -> return $ case toQDatum datum of
            Left err -> Left $ "Error parsing quick-datum '" <> name <> "': " <> err
            Right val -> Right val

qDatumOr :: (DatumMonad m, QDatum a) => a -> Text -> m a
qDatumOr def name = either (const def) id `liftM` qDatum name

class QDatum a where
    toQDatum :: Text -> Either Text a
instance QDatum Text where
    toQDatum = Right
instance QDatum LText where
    toQDatum = Right . fromStrictT
instance QDatum String where
    toQDatum = Right . T.unpack
--instance (Integral a) => QDatum a where
--    toQDatum = STUB

instance IsString Route where
    fromString str = case T.split (==',') (T.pack str) of
            [one] -> mkRoute one
            many -> foldl1 orRoute (map mkRoute many)
        where
        mkRoute = mconcat . map mkSeg . T.split (=='/')
        --FIXME percent-decode
        mkSeg "" = zero
        mkSeg "\0" = zero
        mkSeg "..." = remaining pathKey --FIXME normalize the path (remove //, /./, /../, fail if not all /../ can be removed)
        mkSeg text = case fromJust $ T.uncons text of
            (':', name) -> qRoute name
            ('^', rest) -> error "TODO: regex quick-routes"
            _ -> literal text


newtype MkVault a = MkVault { unMkVault :: State Vault a }
    deriving(Functor, Applicative, Monad)
mkVault :: MkVault () -> Vault
mkVault = flip execState Vault.empty . unMkVault

(===) :: Key a -> a -> MkVault ()
k === v = MkVault $ modify $ Vault.insert k v