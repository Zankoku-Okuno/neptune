module Web.Neptune.Encoding (
	  toStrict
    , fromStrict
    , toStrictT
    , fromStrictT

	, encodeUtf8
    , encodeUtf8L
    , encodeLatin1
    , encodeLatin1L
    , encodePercent
    , encodePercentL
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
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT




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
    
encodeLatin1 :: Text -> ByteString
encodeLatin1 = BS.pack . map encByte . T.unpack
    where
    encByte c | c >= chr 256 = error "Text cannot be encoded in Latin-1."
    encByte c = fromIntegral $ fromEnum c

encodeUtf8L :: LText -> LByteString
encodeUtf8L = LT.encodeUtf8

encodePercentL :: LText -> LByteString
encodePercentL = LBS.pack . concatMap encByte . LBS.unpack . encodeUtf8L

encodeLatin1L :: LText -> LByteString
encodeLatin1L = LBS.pack . map encByte . LT.unpack
    where
    encByte c | c >= chr 256 = error "Text cannot be encoded in Latin-1."
    encByte c = fromIntegral $ fromEnum c

decodeUtf8 :: ByteString -> Text
decodeUtf8 = T.decodeUtf8

decodePercent :: ByteString -> Text
decodePercent = decodeUtf8 . BS.pack . reverse . decByte [] . BS.unpack
    
decodeLatin1 :: ByteString -> Text
decodeLatin1 = T.pack . map (chr . fromIntegral) . BS.unpack

decodeUtf8L :: LByteString -> LText
decodeUtf8L = LT.decodeUtf8

decodePercentL :: LByteString -> LText
decodePercentL = decodeUtf8L . LBS.pack . reverse . decByte [] . LBS.unpack

decodeLatin1L :: LByteString -> LText
decodeLatin1L = LT.pack . map (chr . fromIntegral) . LBS.unpack

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