module Web.Neptune.Action (
      VaultMonad(vault)
    , RequestMonad(request, requests)
    , ReverseMonad(url)
    ) where

import Web.Neptune.Util
import Web.Neptune.Core

import qualified Data.Vault.Lazy as Vault
import Control.Monad.State

instance VaultMonad ActionM where
    vault key = Action $ do
        vault <- hVault <$> get
        return $ key `Vault.lookup` vault

instance RequestMonad ActionM where
    request = hRequest <$> Action get

instance ReverseMonad ActionM where
    url eid args query = do
        s <- hNeptune <$> Action get
        case reverseUrl s eid args query of
            Nothing -> raise $ NoUrlReverse eid args
            Just res -> return res