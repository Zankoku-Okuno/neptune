module Web.Neptune.Format (
      FormatM, Format
    , DatumMonad(datum)
    , RequestMonad(request)
    , ReverseMonad(url)
    , ConfigMonad(config)
    , lbs, bytes, text, encode
    , builderResponse
    , sendfile
    ) where

import Web.Neptune.Core
import Web.Neptune.Escape

import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.Text.Encoding

import qualified Data.Vault.Lazy as Vault
import Control.Monad.Reader

instance DatumMonad FormatM where
    datum key = Format $ do
        vault <- asks hData
        return $ key `Vault.lookup` vault

instance RequestMonad FormatM where
    request = Format $ asks hRequest

instance ConfigMonad FormatM where
    config key = Format $ asks $ Vault.lookup key . nConfig . hNeptune

instance ReverseMonad FormatM where
    url eid args query = do
        s <- Format $ asks hNeptune
        case reverseUrl s eid args query of
            Nothing -> internalError $ "Error: could not reverse url " <> eid
            Just res -> return res

lbs :: LByteString -> Format
lbs = return . LBSResponse

bytes :: ByteString -> Format
bytes = lbs . fromStrict

text :: Text -> Format
text = bytes . encodeUtf8

encode :: (Text -> ByteString) -> Text -> Format
encode codec text = bytes $ codec text

builderResponse :: Builder -> Format
builderResponse = return . BuilderResponse

sendfile :: FilePath -> Format
sendfile = return . FileResponse