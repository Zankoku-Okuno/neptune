{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Web.Neptune.Tools (
      toStrict
    , fromStrict
    , toStrictT
    , fromStrictT

    -- TODO codecs
    , encodeUtf8
    , encodeUtf8L
    , encodePercent
    , encodePercentL
    , decodeUtf8
    , decodeUtf8L
    , decodePercent
    , decodePercentL

    , newKey

    , requests
    , queryAll
    , query
    , attachment

    , datumOr
    , datum_f

    , QDatum(..)
    , qRoute
    , qDatum
    , qDatumOr
    , qDatum_f
    , qDatumOr_f
    , qDatum_ff
    
    , pathKey

    , MkVault
    , mkVault
    , (===)
    ) where

import Web.Neptune.Core
import Web.Neptune.Route
import Web.Neptune.Escape

import System.IO.Unsafe
import Data.Word8
import Data.Char
import Numeric (showHex)
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

encodePercent :: Text -> ByteString
encodePercent = BS.pack . concatMap encByte . BS.unpack . encodeUtf8
    
--encodeLatin1

encodeUtf8L :: LText -> LByteString
encodeUtf8L = LT.encodeUtf8

encodePercentL :: LText -> LByteString
encodePercentL = LBS.pack . concatMap encByte . LBS.unpack . encodeUtf8L

--encodeLatin1L

decodeUtf8 :: ByteString -> Text
decodeUtf8 = T.decodeUtf8

decodePercent :: ByteString -> Text
decodePercent = decodeUtf8 . BS.pack . reverse . decByte [] . BS.unpack
    
--decodeLatin1

decodeUtf8L :: LByteString -> LText
decodeUtf8L = LT.decodeUtf8

decodePercentL :: LByteString -> LText
decodePercentL = decodeUtf8L . LBS.pack . reverse . decByte [] . LBS.unpack

--decodeLatin1L

encByte :: Word8 -> [Word8]
encByte c | _0 <= c && c <= _9 = [c]
encByte c | _A <= c && c <= _Z = [c]
encByte c | _a <= c && c <= _z = [c]
encByte c | c `elem` [_underscore, _hyphen, _period, _tilde] = [c]
encByte c = _percent : if length hex == 1 then _0:hex else hex
    where
    hex :: [Word8]
    hex = fromIntegral . ord <$> showHex c ""

decByte :: [Word8] -> [Word8] -> [Word8]
decByte acc [] = acc
decByte acc (_percent:b:l:rest) = case (fromHex b, fromHex l) of
        (Just b', Just l') -> decByte (16*b' + l' : acc) rest
        _ -> decByte (_percent:acc) (b:l:rest)
    where
    fromHex :: Word8 -> Maybe Word8
    fromHex b | 0x30 <= b && b <= 0x39 = Just $ b - 0x30
    fromHex b | 0x41 <= b && b <= 0x46 = Just $ b - 0x41 + 10
    fromHex b | 0x61 <= b && b <= 0x66 = Just $ b - 0x61 + 10
    fromHex _ = Nothing
decByte acc (c:rest) = decByte (c:acc) rest


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

datum_f :: (DatumMonad m, ResultMonad m) => Key a -> m a
datum_f key = do
    m_x <- datum key
    case m_x of
        Nothing -> internalError "No such datum."
        Just x -> return x


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

qDatum :: (DatumMonad m, QDatum a) => Text -> m (Maybe (Either Text a))
qDatum name = do
    m_datum <- Map.lookup name `liftM` datumOr Map.empty _quickKey
    case m_datum of
        Nothing -> return Nothing
        Just datum -> return . Just $ case toQDatum datum of
            Left err -> Left err
            Right val -> Right val

qDatumOr :: (DatumMonad m, QDatum a) => a -> Text -> m (Either Text a)
qDatumOr def name = fromMaybe (Right def) `liftM` qDatum name

qDatum_f :: (ResultMonad m, DatumMonad m, QDatum a) => Text -> m (Maybe a)
qDatum_f name = do
    me_x <- qDatum name
    case me_x of
        Nothing -> return Nothing
        Just e_x -> case e_x of
            Left err -> internalError $ "Error parsing qDatum " <> name <> ": " <> err
            Right x -> return $ Just x

qDatumOr_f :: (ResultMonad m, DatumMonad m, QDatum a) => a -> Text -> m a
qDatumOr_f def name = do
    me_x <- qDatum name
    case me_x of
        Nothing -> return def
        Just e_x -> case e_x of
            Left err -> internalError $ "Error parsing qDatum " <> name <> ": " <> err
            Right x -> return x

qDatum_ff :: (ResultMonad m, DatumMonad m, QDatum a) => Text -> m a
qDatum_ff name = do
    me_x <- qDatum name
    case me_x of
        Nothing -> internalError $ "Error: no such qDatum: " <> name
        Just e_x -> case e_x of
            Left err -> internalError $ "Error parsing qDatum " <> name <> ": " <> err
            Right x -> return x

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

{- Create Routes from Strings -}
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


{-Easily builds a Vault -}
newtype MkVault a = MkVault { unMkVault :: State Vault a }
    deriving(Functor, Applicative, Monad)
mkVault :: MkVault () -> Vault
mkVault = flip execState Vault.empty . unMkVault

(===) :: Key a -> a -> MkVault ()
k === v = MkVault $ modify $ Vault.insert k v

