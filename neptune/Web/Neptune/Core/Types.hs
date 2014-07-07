{-| The module defines many plain data types that find continual use throughout
    Neptune and in the code of application developers.

    We specifically reject monadic data types from this module; this is what
    distinguishes plain data from computation-as-data.
-}
module Web.Neptune.Core.Types where

import Web.Neptune.Core.Util

import qualified Data.Map as Map
import qualified Data.Vault.Lazy as Vault
import qualified Network.Wai.Parse as Wai
import qualified Network.HTTP.Types as Wai
import qualified Network.HTTP.Media as Wai


-- |Identifier used to select a route during URL reversal.
type EndpointId = Text
-- |Protocol segment of a 'URL'.
type Scheme = ByteString
-- |Hostname or IP address.
type Host = ByteString
-- |Parsed URL path
type PathInfo = [Text]

{-| A structured representation of a URL.

    We don't include a field for password, as that field has
    been deprecated.
-}
data URL = URL
    { _scheme :: Scheme
    , _user :: Maybe ByteString
    , _host :: Host
    , _port :: Maybe Int
    , _path :: PathInfo
    , _query :: Map Text [Parameter]
    , _fragment :: Maybe Text --roughly same as a path segment
    }

-- | Verbs
type Verb = ByteString

-- | A single, specific media type.
type MediaType = Wai.MediaType
-- | A range of media types suitable for content-negotiation.
type AcceptMedia = [Wai.Quality MediaType]
-- | A single, specific language
type Language = Text --STUB
-- | A range of languages suitable for content-negotiation.
type AcceptLang = [Wai.Quality Language]

-- | A parameter in a request (such as passed in the query string if a URI).
type Parameter = ByteString
-- | A file in a request (such as passed as files in a url-encoded form).
type Attachment = Wai.FileInfo LByteString
-- | Application state values (such as set by cookies).
type AppState = ByteString

-- | An expiration time described by some number of seconds into the future.
type Expiry = Integer


{-| A Neptune 'Request' is an abstraction involving only RESTful request metadata and body.
    It is specifically not a lowest-common-denominator.
    
    When serving Neptune over a real protocol, the server will first need to translate the
    protocol request into a Neptune request. When fields are not directly supported, they
    will need to be synthesized. When fields are not supported by the protocol at all, then
    appropriate placeholders should be given.

-}
data Request = Request
    { resource :: PathInfo
    , verb :: Verb
    , acceptType :: AcceptMedia
    , acceptLang :: AcceptLang
    , appState :: Map Text AppState
    , queries :: Map Text [Parameter]
    , attachments :: Map Text [Attachment]
    , requestBody :: Maybe (MediaType, LByteString)
    , requestData :: Vault
    }

{-| A Neptune 'Response' is an abstraction involving only RESTful response metadata and body.
    It is specifically not a lowest-common-denominator.

    When serving Neptune over a real protocol, the server will need to translate the Neptune
    response into a protocol response. When fields are not directly supported, they
    will need to be synthesized. When fields are not supported by the protocol at all, then
    appropriate placeholders should be given.
-}
data Response = Response
    { mimetype :: Maybe MediaType
    , language :: Maybe Language
    , cacheFor :: Maybe Expiry
    , updateAppState :: Map Text (Maybe (AppState, Maybe Expiry))
    , body :: ResponseBody
    }
              | CustomResponse Text Vault
              | Redirect       RedirectReason URL
              | BadContent     [MediaType] -- the types the app can consume
              | BadResource    
              | BadVerb        [Verb]
              | BadAccept      [MediaType] -- the types the app can produce
              | BadLanguage    
              | BadPermissions  
              | Timeout        DiffTime
              | InternalError  Text

{-| For greater interoperability, the body of a 'Response' can be one of
    many textual data types.
-}
data ResponseBody = LBSResponse LByteString
                  | BuilderResponse Builder
                  | FileResponse FilePath

{-| The cause for a redirect is meaningful to caches and can be
    meaningful to user agents deciding on the next course of action.
-}
data RedirectReason = Created | Moved | Temporary


instance IsString ResponseBody where
    fromString = LBSResponse . fromString

instance Default Response where
    def = Response
        { mimetype = Nothing
        , language = Nothing
        , cacheFor = Nothing
        , updateAppState = Map.empty
        , body = ""
        }

