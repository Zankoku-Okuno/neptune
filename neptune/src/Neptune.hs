{-#LANGUAGE TupleSections #-}
module Neptune
    ( module Neptune.Core
    , module Neptune.Route
    , module Neptune.Query
    , module Neptune.Verb
    , module Neptune.Negotiate
    , module Neptune.Error
    , neptune
    ) where

import ClassyPrelude
import Neptune.Core
import Neptune.Route
import Neptune.Query
import Neptune.Verb
import Neptune.Negotiate
import Neptune.Error


neptune :: [Route] -> ErrorReporter -> NeptuneApp
neptune handlers onError req@Request{..} = do
    let pipeline = case dispatch handlers resourceId of
            Nothing -> throw BadResource
            Just handler -> handler req
    let main = pipeline `catch` (pure . onError req)
    main `catch` (pure . fallbackErrorResponse)






