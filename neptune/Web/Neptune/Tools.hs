{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
module Web.Neptune.Tools (
      module Web.Neptune.Tools.Encoding
    , module Web.Neptune.Tools.Url
    , module Web.Neptune.Tools.Vault
    ) where

import Web.Neptune.Core
import Web.Neptune.Route
import Web.Neptune.Tools.Encoding
import Web.Neptune.Tools.Url
import Web.Neptune.Tools.Vault

import qualified Data.Text as T


{- Create 'Route's from 'String's -}
instance IsString Route where
    fromString str = case T.split (==',') (T.pack str) of
            [option] -> mkRoute option
            options -> foldl1 orRoute (map mkRoute options)
        where
        mkRoute = mconcat . map mkSeg . normalizePath . T.split (=='/')
        --FIXME percent-decode
        mkSeg "" = zero
        mkSeg "\0" = zero
        mkSeg "..." = remaining pathKey
        mkSeg text = case fromJust $ T.uncons text of
            (':', name) -> qRoute name
            ('^', rest) -> error "TODO: regex quick-routes"
            _ -> literal text


