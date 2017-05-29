{-# LANGUAGE TypeSynonymInstances #-}
module Neptune.Verb where

import ClassyPrelude
import Neptune.Core


verbs :: Verb -> [(Verb, a)] -> Either Error a
verbs verb options = go [] options
    where
    go allowed [] = Left . BadVerb $ reverse allowed
    go allowed ((v, x) : rest)
        | v == verb = Right x
        | otherwise = go (v:allowed) rest

verb :: Verb -> (Verb, a) -> Either Error a
verb verb option = verbs verb [option]

infixr 0 >:
(>:) :: a -> b -> (a, b)
a >: b  = (a, b)