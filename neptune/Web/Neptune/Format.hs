module Web.Neptune.Format (
      ParamMonad(param)
    , RequestMonad(request, requests)
    , ReverseMonad(url)
    ) where

import Web.Neptune.Util
import Web.Neptune.Core

import Data.Vault.Lazy (Vault, Key)
import qualified Data.Vault.Lazy as Vault
import Control.Monad.Reader

instance ParamMonad FormatM where
    param key = Format $ do
        vault <- asks hParams
        return $ key `Vault.lookup` vault

instance RequestMonad FormatM where
    request = Format $ asks hRequest

instance ReverseMonad FormatM where
    url eid args = do
        s <- Format $ asks hNeptune
        case reverseUrl s eid args of
            Nothing -> raise $ NoUrlReverse eid args
            Just res -> return res