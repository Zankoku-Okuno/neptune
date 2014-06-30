{-# LANGUAGE OverloadedStrings #-}
module Web.Neptune.Types (
    -- * Requests and Responses
      Request(..)
    , Response(..)
    , ResponseBody(..)
    , Application
    , Middleware
    -- * Basic Types
    -- ** URIs
    , EndpointId
    , Location
    , Domain
    , PathInfo
    -- ** Verbs
    , Verb
    -- ** Content
    , MediaType
    , AcceptMedia
    , Language
    , AcceptLang
    -- ** Request Data
    , Parameter
    , Attachment
    , AppState
    -- ** Expiration
    , Expiry
    ) where

import Web.Neptune.Util

import qualified Data.Map as Map
import qualified Data.Vault.Lazy as Vault
import qualified Network.Wai.Parse as Wai
import qualified Network.HTTP.Types as Wai
import qualified Network.HTTP.Media as Wai

-- | A Neptune application takes a request and generates a response,
--   which may require performing side-effects.
type Application = Request -> IO Response
-- | Wrap an application with additional functionality.
type Middleware = Application -> Application


-- | Identifier used to select a route during URL reversal.
type EndpointId = Text
-- | A domain name (optionally including protocol, port, and any leading path info).
type Domain = Text
-- | Parsed URL path
type PathInfo = [Text]
-- | URIs
type Location = (Domain, PathInfo, Map Text Parameter)

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
    , reqBody :: Maybe (MediaType, LByteString)
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
              | EmptyResponse  Response Text --the Text is like an error code
              | Redirect       Location Bool --the Bool means it is permanent
              | BadContent     [MediaType] -- the types the app can consume
              | BadResource    
              | BadVerb        [Verb]
              | BadAccept      [MediaType] -- the types the app can produce
              | BadLanguage    
              | BadPermissions  
              | Timeout        DiffTime
              | InternalError  
              | NoUrlReverse   EndpointId Vault
              --TODO? a Debug response

data ResponseBody = LBSResponse LByteString
                  | BuilderResponse Builder
                  | FileResponse FilePath

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

