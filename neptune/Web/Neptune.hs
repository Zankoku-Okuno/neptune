module Web.Neptune (
    -- * Neptune Applications
      Neptune
    , NeptuneM
    , Application, Middleware
    , Request(..)
    , Response(..)
    , ResponseBody(..)
    , NeptuneServer(..)
    , compileNeptune
    , serve
    
    -- * Request Handling Pipeline
    -- * Routing
    , module Web.Neptune.Route
    -- ** Actions
    , module Web.Neptune.Action
    -- ** Formatting
    , module Web.Neptune.Format
    -- ** Early Escape
    , module Web.Neptune.Escape
    
    -- * Types
    -- ** High-level Protocol Elements
    -- ** Low-level Protocol Elements
    , URL
    , Verb
    , MediaType
    , Language
    , AcceptMedia
    , AcceptLang
    , Parameter
    , Attachment
    , AppState
    , Expiry

    -- * Convenience Monad Classes
    , ResultMonad(..)
    , Result
    , RequestMonad(..)
    , requests
    , queryAll
    , query
    , attachment
    , DatumMonad(..)
    , pathKey
    , datumOr
    , datum_f
    , ReverseMonad(..)
    , ConfigMonad(..)
    
    -- * Re-Exports
    -- ** Maybe Tools
    , module Data.Maybe
    , nothing
    , fromMaybeM
    , module Data.Default
    -- ** String Manipulation
    , IsString(fromString)
    , ByteString, LByteString
    , Text, LText
    , Builder
    -- ** Containers
    , Map, Vault, Key
    , module Web.Neptune.Tools.Vault
    , softInsert
    -- ** Categories
    , module Data.Monoid
    , module Control.Applicative
    , module Control.Monad
    , module Control.Monad.Trans
    ) where

import Data.Default
import Data.Maybe
import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Web.Neptune.Core

import Web.Neptune.Route
import Web.Neptune.Action
import Web.Neptune.Format
import Web.Neptune.Escape
import Web.Neptune.Tools
import Web.Neptune.Tools.Vault


-- | A Neptune application takes a request and generates a response,
--   which may require performing side-effects.
type Application = Request -> IO Response
-- | Wrap an application with additional functionality.
type Middleware = Application -> Application

compileNeptune :: Vault -> URL -> Neptune -> NeptuneServer
compileNeptune config prepath = execNeptune prepath . buildNeptune config

{-| Turn a compiled Neptune monad ('buildNeptune') into a real application server.

    In combination with 'buildNeptune' and custom to/fromX functions, it should be
    straightforward to serve a Neptune application over any suitable protocol.
-}
serve :: NeptuneServer -> Application
serve neptune = app
    where
    app request = runPipeline $ do
        let routingState = RS { rRequest = request
                              , rPath = path request
                              , rData = requestData request
                              , rNeptune = neptune
                              }
        m_route <- runRoutesM $ evalHandlers routingState (nHandlers neptune)
        (vault, action) <- case m_route of
            Left [] -> raise BadResource
            Left allowed -> raise $ BadVerb allowed
            Right route -> return route
        let handlingState = HS { hRequest = request
                               , hData = vault
                               , hResponse = def
                               , hNeptune = neptune
                               }
        (formats, state) <- runActionM handlingState action
        let acceptable = fst <$> formats
        (mimetype, renderer) <- maybe (raise $ BadAccept acceptable) return $
            negotiate (acceptType request) formats
        let state' = if mimetype == "*/*"
                        then state
                        else state { hResponse = (hResponse state) {mimetype = Just mimetype} }
        body <- runFormatM state' renderer
        return $ (hResponse state') { body = body }
    runPipeline :: ResultT IO Response -> IO Response
    runPipeline x = runResultT x >>= \res ->
        return $ case res of { Normal x' -> x'; Alternate x' -> x' }