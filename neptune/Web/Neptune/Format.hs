module Web.Neptune.Format (
    -- * Building Response Bodies
      Format
    , FormatM
    , lbs, bytes, text, encode
    , builderResponse
    , sendfile
    , RequestMonad(..)
    , DatumMonad(..)
    , ConfigMonad(..)
    , ReverseMonad(..)
    -- * Content Negotiation
    , negotiate
    , i12izer
    ) where


import Web.Neptune.Core
import Web.Neptune.Convenience

import Data.ByteString.Lazy (fromStrict)
import Data.Text.Encoding


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
encode codec = bytes . codec

-- |Create a response body from a 'Builder'.
builderResponse :: Builder -> Format
builderResponse = return . BuilderResponse

-- |Create a response body from a file.
--
--  It is up to the server interface to decide how to encode the file contents,
--  but it is recommended that the file be transferred byte-for-byte.
sendfile :: FilePath -> Format
sendfile = return . FileResponse

