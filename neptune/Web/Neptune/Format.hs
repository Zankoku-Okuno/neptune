module Web.Neptune.Format (
      DatumMonad(datum)
    , RequestMonad(request)
    , ReverseMonad(url)
    , ConfigMonad(config)
    , lbs, bytes, text, encode
    ) where

import Web.Neptune.Core

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
            Nothing -> raise $ NoUrlReverse eid args
            Just res -> return res

lbs :: LByteString -> Format
lbs = return . LBSResponse

bytes :: ByteString -> Format
bytes = lbs . fromStrict

text :: Text -> Format
text = bytes . encodeUtf8

encode :: (Text -> ByteString) -> Text -> Format
encode codec text = bytes $ codec text

