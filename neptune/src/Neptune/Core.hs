module Neptune.Core
    ( -- * Basic RESTful data
      Verb
    , Query
    , MediaType
    , Accept
    , Quality
    -- ** Synonyms
    , Location
    , Content

    -- * Requests and Responses
    , NeptuneApp
    , Request(..)
    , Response(..)
    , Negotiation(..)
    , Error(..)
    ) where

import ClassyPrelude

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import qualified Network.HTTP.Types as Http -- FIXME don't rely on http explicitly
import Network.HTTP.Media


type Verb = Http.Method
type Location = ([Text], Query) -- FIXME use a proper url type
type Query = Map BS.ByteString [BS.ByteString] -- FIXME? convert to text -- FIXME? use lazy bytestrings
type Content = (MediaType, LBS.ByteString)


type NeptuneApp = Request -> IO Response


data Request = Request
    { resourceId :: Location
    , method :: Http.Method
    , negotiation :: Negotiation
    }
data Negotiation = Negotiation
    { acceptMedia :: [Quality MediaType]
    -- TODO accept-language
    -- TODO accept-encoding
    }

data Response = Response
    { status :: Http.Status
    , responseBody :: Maybe Content
    }
data Error
    = BadResource
    | BadVerb [Http.Method]
    | BadMedia [MediaType]
    deriving (Show)
instance Exception Error
