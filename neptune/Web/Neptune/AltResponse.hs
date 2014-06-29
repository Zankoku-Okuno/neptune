module Web.Neptune.AltResponse (
      handleBadContent
    , handleBadResource
    , handleBadMethod
    , handleBadAccept
    , handleBadLanguage
    , handleBadPermissions
    , handleNoUrlReverse
    , handleTimeout
    , handleInternalError
    ) where

import Web.Neptune.Util
import Web.Neptune.Core

import Data.Time.Clock
import Control.Monad.State


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
handleBadMethod :: MediaType -> ([Method] -> LByteString) -> Neptune
handleBadMethod media format = Neptune $ modify $ \s -> s {
    nErrorHandlers = (nErrorHandlers s) {
        ehBadMethod = (ehBadMethod . nErrorHandlers) s ++ [(media, format)] }
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
handleInternalError :: MediaType -> LByteString -> Neptune
handleInternalError media format = Neptune $ modify $ \s -> s {
    nErrorHandlers = (nErrorHandlers s) {
        ehInternalError = (ehInternalError . nErrorHandlers) s ++ [(media, format)] }
    }
handleNoUrlReverse :: MediaType -> (EndpointId -> Vault -> LByteString) -> Neptune
handleNoUrlReverse media format = Neptune $ modify $ \s -> s {
    nErrorHandlers = (nErrorHandlers s) {
        ehNoUrlReverse = (ehNoUrlReverse . nErrorHandlers) s ++ [(media, format)] }
    }