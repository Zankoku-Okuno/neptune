module Web.Neptune.Format (
      ParamMonad(param)
    , RequestMonad(request, requests)
    , ReverseMonad(url)
    ) where

import Web.Neptune.Util
import Web.Neptune.Core

import qualified Data.Vault.Lazy as Vault
import Control.Monad.Reader

instance ParamMonad FormatM where
    param key = Format $ do
        vault <- asks hParams
        return $ key `Vault.lookup` vault

instance RequestMonad FormatM where
    request = Format $ asks hRequest

instance ReverseMonad FormatM where
    url eid args query = do
        s <- Format $ asks hNeptune
        case reverseUrl s eid args query of
            Nothing -> raise $ NoUrlReverse eid args
            Just res -> return res