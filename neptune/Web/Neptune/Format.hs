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

-- |Create response body from a lazy ByteString.
lbs :: LByteString -> Format
lbs = return . LBSResponse

-- |Create response body from a strict ByteString.
bytes :: ByteString -> Format
bytes = lbs . fromStrict

-- |Create Utf-8 encoded response body from strict Text.
text :: Text -> Format
text = bytes . encodeUtf8

-- |Create a response body from strict Text using a user-supplied codec. C.f. 'text'.
encode :: (Text -> ByteString) -> Text -> Format
encode codec text = bytes $ codec text

-- |Create a response body from a 'Builder'.
builderResponse :: Builder -> Format
builderResponse = return . BuilderResponse

-- |Create a response body from a file.
--
--  It is up to the server interface to decide how to encode the file contents,
--  but it is recommended that the file be transferred byte-for-byte.
sendfile :: FilePath -> Format
sendfile = return . FileResponse