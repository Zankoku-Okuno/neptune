module Web.Neptune.Escape (
      redirect
    , moved
    , created
    , notFound
    , notPermitted
    , internalError
    , customResponse
    
    , handleBadContent
    , handleBadResource
    , handleBadVerb
    , handleBadAccept
    , handleBadLanguage
    , handleBadPermissions
    , handleTimeout
    , handleInternalError
    ) where

import Web.Neptune.Core

import Control.Monad.State


customResponse :: ResultMonad m => Text -> Vault -> m a
customResponse cause = raise . CustomResponse cause

-- |Escape because the requested new resource was successfully created on the server.
created :: ResultMonad m => URL -> m a
created = raise . Redirect Created

-- |Escape with a temporary redirect.
redirect :: ResultMonad m => URL -> m a
redirect = raise . Redirect Temporary

-- |Escape with a permanent (cachable) redirect.
moved :: ResultMonad m => URL -> m a
moved = raise . Redirect Moved

-- |Error due to missing resources.
notFound :: ResultMonad m => m a
notFound = raise $ BadResource

-- |Error due to insufficient authorization.
notPermitted :: ResultMonad m => m a
notPermitted = raise $ BadPermissions

-- |Error due to system malfunction or programmer error.
internalError :: ResultMonad m => Text -> m a
internalError = raise . InternalError


-- |Set up a custom 'BadContent' error response renderer.
handleBadContent :: MediaType -> ([MediaType] -> LByteString) -> Neptune
handleBadContent media format = Neptune $ modify $ \s -> s {
    nlErrorHandlers = (nlErrorHandlers s) {
        ehBadContent = (ehBadContent . nlErrorHandlers) s ++ [(media, format)] }
    }

-- |Set up a custom 'BadResource' error response renderer.
handleBadResource :: MediaType -> LByteString -> Neptune
handleBadResource media format = Neptune $ modify $ \s -> s {
    nlErrorHandlers = (nlErrorHandlers s) {
        ehBadResource = (ehBadResource . nlErrorHandlers) s ++ [(media, format)] }
    }

-- |Set up a custom 'BadVerb' error response renderer.
handleBadVerb :: MediaType -> ([Verb] -> LByteString) -> Neptune
handleBadVerb media format = Neptune $ modify $ \s -> s {
    nlErrorHandlers = (nlErrorHandlers s) {
        ehBadVerb = (ehBadVerb . nlErrorHandlers) s ++ [(media, format)] }
    }

-- |Set up a custom 'BadAccept' error response renderer.
handleBadAccept :: MediaType -> ([MediaType] -> LByteString) -> Neptune
handleBadAccept media format = Neptune $ modify $ \s -> s {
    nlErrorHandlers = (nlErrorHandlers s) {
        ehBadAccept = (ehBadAccept . nlErrorHandlers) s ++ [(media, format)] }
    }

-- |Set up a custom 'BadLanguage' error response renderer.
handleBadLanguage :: MediaType -> (LByteString) -> Neptune
handleBadLanguage media format = Neptune $ modify $ \s -> s {
    nlErrorHandlers = (nlErrorHandlers s) {
        ehBadLanguage = (ehBadLanguage . nlErrorHandlers) s ++ [(media, format)] }
    }

-- |Set up a custom 'BadPermissions' error response renderer.
handleBadPermissions :: MediaType -> LByteString -> Neptune
handleBadPermissions media format = Neptune $ modify $ \s -> s {
    nlErrorHandlers = (nlErrorHandlers s) {
        ehBadPermissions = (ehBadPermissions . nlErrorHandlers) s ++ [(media, format)] }
    }

-- |Set up a custom 'Timeout' error response renderer.
handleTimeout :: MediaType -> (DiffTime -> LByteString) -> Neptune
handleTimeout media format = Neptune $ modify $ \s -> s {
    nlErrorHandlers = (nlErrorHandlers s) {
        ehTimeout = (ehTimeout . nlErrorHandlers) s ++ [(media, format)] }
    }

-- |Set up a custom 'InternalError' response renderer.
handleInternalError :: MediaType -> (Text -> LByteString) -> Neptune
handleInternalError media format = Neptune $ modify $ \s -> s {
    nlErrorHandlers = (nlErrorHandlers s) {
        ehInternalError = (ehInternalError . nlErrorHandlers) s ++ [(media, format)] }
    }
