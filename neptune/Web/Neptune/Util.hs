module Web.Neptune.Util (
      module X
    , ByteString, Text
    , LByteString, LText
    , IsString(..)
    , Map, Vault, Key
    
    , nothing
    , fromMaybeM
    , softInsert

    , encodeUrl
    , decodeUrl
    ) where

import Data.Default as X
import Data.Maybe as X
import Control.Monad.Maybe
import Data.Map (Map)
import Data.Vault.Lazy (Vault, Key)
import qualified Data.Map as M

import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy as LBS

import qualified Data.ByteString as BS
import Numeric (showHex)
import Data.String (IsString(..))
import Data.Text.Encoding
import Data.Word8 as Word8

import Data.Monoid as X
import Control.Applicative as X
import Control.Monad as X


type LText = LT.Text
type LByteString = LBS.ByteString


fromMaybeM :: (Monad m) => m a -> m (Maybe a) -> m a
fromMaybeM def x = maybe def return =<< x

nothing :: (Monad m) => MaybeT m a
nothing = MaybeT $ return Nothing

softInsert :: (Ord k) => k -> v -> M.Map k v -> M.Map k v
softInsert key val map =
    if key `M.member` map
        then map
        else M.insert key val map


encodeUrl :: [Word8] -> Text -> ByteString
encodeUrl extra = BS.pack . concatMap (encodeUrlByte extra) . BS.unpack . encodeUtf8

encodeUrlByte :: [Word8] -> Word8 -> [Word8]
encodeUrlByte extra c | c <= 32 || c >= 127 
                     || c == _percent
                     || c `elem` extra
    = (_percent:) . BS.unpack . fromString . toHex $ c
    where
    toHex c = if length str == 1 then '0':str else str
        where str = showHex c ""
encodeUrlByte _ c = [c]

decodeUrl :: ByteString -> Text
decodeUrl = decodeUtf8 . BS.pack . reverse . go [] . BS.unpack
    where
    go :: [Word8] -> [Word8] -> [Word8]
    go acc [] = acc
    go acc (_percent:b:l:rest) = case (fromHex b, fromHex l) of
        (Just b', Just l') -> go (fromIntegral (16*b' + l') : acc) rest
        _ -> go (_percent:acc) (b:l:rest)
    go acc (c:rest) = go (c:acc) rest
    fromHex :: Word8 -> Maybe Int
    fromHex 0x30 = Just 0
    fromHex 0x31 = Just 1
    fromHex 0x32 = Just 2
    fromHex 0x33 = Just 3
    fromHex 0x34 = Just 4
    fromHex 0x35 = Just 5
    fromHex 0x36 = Just 6
    fromHex 0x37 = Just 7
    fromHex 0x38 = Just 8
    fromHex 0x39 = Just 9
    fromHex 0x61 = Just 10
    fromHex 0x62 = Just 11
    fromHex 0x63 = Just 12
    fromHex 0x64 = Just 13
    fromHex 0x65 = Just 14
    fromHex 0x66 = Just 15
    fromHex 0x41 = Just 10
    fromHex 0x42 = Just 11
    fromHex 0x43 = Just 12
    fromHex 0x44 = Just 13
    fromHex 0x45 = Just 14
    fromHex 0x46 = Just 15
    fromHex _ = Nothing
    
