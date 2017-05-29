module Neptune.Query where

import ClassyPrelude
import Neptune.Core

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS


query_queryAll :: Query -> BS.ByteString -> [BS.ByteString]
query_queryAll q k = fromMaybe [] $ lookup k q

query_queryOne :: Query -> BS.ByteString -> Maybe BS.ByteString
query_queryOne q k = listToMaybe $ query_queryAll q k
