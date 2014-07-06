module Web.Neptune.Types (
    -- * Requests and Responses
      Request(..)
    , Response(..)
    , ResponseBody()
    , Application
    , Middleware
    -- * Basic Types
    -- ** URIs
    , EndpointId
    , URL
    , Scheme
    , Host
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

import Web.Neptune.Core.Types

-- | A Neptune application takes a request and generates a response,
--   which may require performing side-effects.
type Application = Request -> IO Response
-- | Wrap an application with additional functionality.
type Middleware = Application -> Application
