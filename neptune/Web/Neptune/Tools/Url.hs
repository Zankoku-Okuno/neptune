module Web.Neptune.Tools.Url (
      URL
    , Scheme, Host, PathInfo, Parameter
    , showURL
    , normalizePath
    -- * URL Building
    , simpleUrl
    , urlUser
    , urlPort
    , urlPath
    , urlQuery
    , urlHash
    ) where


import Web.Neptune.Core.Util
import Web.Neptune.Core.Types
import Web.Neptune.Core.Url
import Web.Neptune.Tools.Encoding

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Map as Map


showURL :: URL -> ByteString
showURL url = mconcat [ _scheme url, "://"
                        , maybe "" (<> "@") (_user url)
                        , _host url
                        , maybe "" ((":" <>) . showPort) (_port url)
                        , mconcat $ ("/" <>) . encodePercent <$> _path url
                        , raw_query (_query url)
                        , maybe "" (("#" <>) . encodePercent) (_fragment url)
                        ]
    where
    raw_query :: Map Text [Parameter] -> ByteString
    raw_query query = case Map.toList query of
        [] -> ""
        queryList -> "?" <> BS.intercalate "&" (formatParam <$> queryList)
    formatParam :: (Text, [Parameter]) -> ByteString
    formatParam (k, vs) = BS.intercalate "&" $ format1Param k <$> vs
    format1Param :: Text -> Parameter -> ByteString
    format1Param k v = encodePercent k <> "=" <> v
    showPort = encodeLatin1 . T.pack . show