{-| This module defines functions for transforming between different text data formats.

    In particular, we target strict and lazy Text and ByteString. Conversions between 
    Text and ByteString require selecting a codec.

    Percent encoding, as we perform it in this module, first performs Utf-8 encoding. Any
    octets in @[a-zA-Z0-9_.~-]@ are passed through without transformations. All other
    octets are encoded as @%XX@, with @XX@ as its two-digit hexadecimal representation.

    Percent decoding is very permissive. If a @%XX@ sequence can be decoded as an octet,
    then it is. Otherwise, we treat the @%@ as a regular character and continue. Otherwise,
    whenever octets outside of @[a-zA-Z0-9_.~-]@ are in the character stream, they are passed
    through. The final octet-stream is then Utf-8 decoded.
-}
module Web.Neptune.Tools.Encoding (
	-- * Strict and Lazy
      toStrictBS
    , fromStrictBS
    , toStrictT
    , fromStrictT
    -- * ByteString and Text
    -- ** Encode
	, encodeUtf8
    , encodeUtf8L
    , encodeLatin1
    , encodeLatin1L
    , encodePercent
    , encodePercentL
    -- ** Decode
    , decodeUtf8
    , decodeUtf8L
    , decodeLatin1
    , decodeLatin1L
    , decodePercent
    , decodePercentL
    ) where

import Web.Neptune.Core.Util

import Data.Word8
import Data.Char
import Numeric (showHex)

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as LT

-- |Lazy Bytestring to strict Bytestring.
toStrictBS :: LByteString -> ByteString
toStrictBS = LBS.toStrict

-- |Strict Bytestring to lazy Bytestring.
fromStrictBS :: ByteString -> LByteString
fromStrictBS = LBS.fromStrict

-- |Lazy Text to strict Text.
toStrictT :: LText -> Text
toStrictT = LT.toStrict

-- |Strict Text to lazy Text.
fromStrictT :: Text -> LText
fromStrictT = LT.fromStrict


-- |Encode strict Text using Utf-8, a superset of ASCII.
encodeUtf8 :: Text -> ByteString
encodeUtf8 = T.encodeUtf8

-- |Encode strict Text using percent encoding.
encodePercent :: Text -> ByteString
encodePercent = BS.pack . concatMap encBytePercent . BS.unpack . encodeUtf8

-- |Encode strict Text using Latin-1, a superset of ASCII.
encodeLatin1 :: Text -> ByteString
encodeLatin1 = BS.pack . map encByteLatin1 . T.unpack

-- |Encode lazy Text using Utf-8, a superset of ASCII.
encodeUtf8L :: LText -> LByteString
encodeUtf8L = LT.encodeUtf8

-- |Encode lazy Text using percent encoding.
encodePercentL :: LText -> LByteString
encodePercentL = LBS.pack . concatMap encBytePercent . LBS.unpack . encodeUtf8L

-- |Encode lazy Text using Latin-1, a superset of ASCII.
encodeLatin1L :: LText -> LByteString
encodeLatin1L = LBS.pack . map encByteLatin1 . LT.unpack

-- |Decode a Utf-8 encoded strict ByteString.
decodeUtf8 :: ByteString -> Text
decodeUtf8 = T.decodeUtf8

-- |Decode a percent-encoded strict ByteString.
decodePercent :: ByteString -> Text
decodePercent = decodeUtf8 . BS.pack . reverse . decByte [] . BS.unpack

-- |Decode a Latin-1 encoded strict ByteString.
decodeLatin1 :: ByteString -> Text
decodeLatin1 = T.pack . map (chr . fromIntegral) . BS.unpack

-- |Decode a Utf-8 encoded lazy ByteString.
decodeUtf8L :: LByteString -> LText
decodeUtf8L = LT.decodeUtf8

-- |Decode a percent-encoded lazy ByteString.
decodePercentL :: LByteString -> LText
decodePercentL = decodeUtf8L . LBS.pack . reverse . decByte [] . LBS.unpack

-- |Decode a Latin-1 encoded lazy ByteString.
decodeLatin1L :: LByteString -> LText
decodeLatin1L = LT.pack . map (chr . fromIntegral) . LBS.unpack


encBytePercent :: Word8 -> [Word8]
encBytePercent c | _0 <= c && c <= _9 = [c]
encBytePercent c | _A <= c && c <= _Z = [c]
encBytePercent c | _a <= c && c <= _z = [c]
encBytePercent c | c `elem` [_underscore, _hyphen, _period, _tilde] = [c]
encBytePercent c = _percent : if length hex == 1 then _0:hex else hex
    where
    hex :: [Word8]
    hex = fromIntegral . ord <$> showHex c ""

decByte :: [Word8] -> [Word8] -> [Word8]
decByte acc [] = acc
decByte acc (_percent:high:low:rest) = case (fromHex high, fromHex low) of
        (Just high', Just low') -> decByte (16*high' + low' : acc) rest
        _ -> decByte (_percent:acc) (high:low:rest)
    where
    fromHex :: Word8 -> Maybe Word8
    fromHex b | 0x30 <= b && b <= 0x39 = Just $ b - 0x30
    fromHex b | 0x41 <= b && b <= 0x46 = Just $ b - 0x41 + 10
    fromHex b | 0x61 <= b && b <= 0x66 = Just $ b - 0x61 + 10
    fromHex _ = Nothing
decByte acc (c:rest) = decByte (c:acc) rest

encByteLatin1 :: Char -> Word8
encByteLatin1 c | c >= chr 256 = error "Text cannot be encoded in Latin-1."
encByteLatin1 c = fromIntegral $ fromEnum c