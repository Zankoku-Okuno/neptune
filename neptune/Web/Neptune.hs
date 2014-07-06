module Web.Neptune (
      module Data.Maybe
    , module Data.Monoid
    , module Control.Applicative
    , module Control.Monad
    , module Control.Monad.Trans
    
    , IsString(fromString)
    , ByteString, LByteString
    , Text, LText
    , Builder

    , Map, Vault, Key

    , module Web.Neptune.Types
    , Neptune, NeptuneM
    , module Web.Neptune.Route
    , module Web.Neptune.Action
    , module Web.Neptune.Format
    , module Web.Neptune.Escape
    , module Web.Neptune.Url
    , module Web.Neptune.Tools

    , serve
    ) where

import Data.Maybe
import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Web.Neptune.Core

import Web.Neptune.Types
import Web.Neptune.Route
import Web.Neptune.Action
import Web.Neptune.Format
import Web.Neptune.Escape
import Web.Neptune.Url
import Web.Neptune.Tools


{-| Turn a compiled Neptune monad ('buildNeptune') into a real application server.

    In combination with 'buildNeptune' and custom to/fromX functions, it should be
    straightforward to serve a Neptune application over any suitable protocol.
-}
serve :: NeptuneState -> Application
serve neptune = app
    where
    app request = runPipeline $ do
        let routingState = RS { rRequest = request
                              , rPath = resource request
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
        (mimetype, format) <- maybe (raise $ BadAccept acceptable) return $
            negotiate (acceptType request) formats
        let state' = if mimetype == "*/*"
                        then state
                        else state { hResponse = (hResponse state) {mimetype = Just mimetype} }
        body <- runFormatM state' format
        return $ (hResponse state') { body = body }
    runPipeline :: ResultT IO Response -> IO Response
    runPipeline x = runResultT x >>= \res ->
        return $ case res of { Normal x -> x; Alternate x -> x }