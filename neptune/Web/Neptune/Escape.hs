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
    , handleNoUrlReverse
    , handleTimeout
    , handleInternalError
    ) where

import Web.Neptune.Core

import Control.Monad.State


customResponse :: ResultMonad m => Text -> Vault -> m a
customResponse cause = raise . CustomResponse cause

created :: ResultMonad m => Location -> m a
created = raise . Redirect Created

redirect :: ResultMonad m => Location -> m a
redirect = raise . Redirect Temporary

moved :: ResultMonad m => Location -> m a
moved = raise . Redirect Moved

notFound :: ResultMonad m => m a
notFound = raise $ BadResource

notPermitted :: ResultMonad m => m a
notPermitted = raise $ BadPermissions

internalError :: ResultMonad m => Text -> m a
internalError = raise . InternalError


handleBadContent :: MediaType -> ([MediaType] -> LByteString) -> Neptune
handleBadContent media format = Neptune $ modify $ \s -> s {
    nErrorHandlers = (nErrorHandlers s) {
        ehBadContent = (ehBadContent . nErrorHandlers) s ++ [(media, format)] }
    }
handleBadResource :: MediaType -> LByteString -> Neptune
handleBadResource media format = Neptune $ modify $ \s -> s {
    nErrorHandlers = (nErrorHandlers s) {
        ehBadResource = (ehBadResource . nErrorHandlers) s ++ [(media, format)] }
    }
handleBadVerb :: MediaType -> ([Verb] -> LByteString) -> Neptune
handleBadVerb media format = Neptune $ modify $ \s -> s {
    nErrorHandlers = (nErrorHandlers s) {
        ehBadVerb = (ehBadVerb . nErrorHandlers) s ++ [(media, format)] }
    }
handleBadAccept :: MediaType -> ([MediaType] -> LByteString) -> Neptune
handleBadAccept media format = Neptune $ modify $ \s -> s {
    nErrorHandlers = (nErrorHandlers s) {
        ehBadAccept = (ehBadAccept . nErrorHandlers) s ++ [(media, format)] }
    }
handleBadLanguage :: MediaType -> (LByteString) -> Neptune
handleBadLanguage media format = Neptune $ modify $ \s -> s {
    nErrorHandlers = (nErrorHandlers s) {
        ehBadLanguage = (ehBadLanguage . nErrorHandlers) s ++ [(media, format)] }
    }
handleBadPermissions :: MediaType -> LByteString -> Neptune
handleBadPermissions media format = Neptune $ modify $ \s -> s {
    nErrorHandlers = (nErrorHandlers s) {
        ehBadPermissions = (ehBadPermissions . nErrorHandlers) s ++ [(media, format)] }
    }
handleTimeout :: MediaType -> (DiffTime -> LByteString) -> Neptune
handleTimeout media format = Neptune $ modify $ \s -> s {
    nErrorHandlers = (nErrorHandlers s) {
        ehTimeout = (ehTimeout . nErrorHandlers) s ++ [(media, format)] }
    }
handleInternalError :: MediaType -> (Text -> LByteString) -> Neptune
handleInternalError media format = Neptune $ modify $ \s -> s {
    nErrorHandlers = (nErrorHandlers s) {
        ehInternalError = (ehInternalError . nErrorHandlers) s ++ [(media, format)] }
    }
handleNoUrlReverse :: MediaType -> (EndpointId -> Vault -> LByteString) -> Neptune
handleNoUrlReverse media format = Neptune $ modify $ \s -> s {
    nErrorHandlers = (nErrorHandlers s) {
        ehNoUrlReverse = (ehNoUrlReverse . nErrorHandlers) s ++ [(media, format)] }
    }