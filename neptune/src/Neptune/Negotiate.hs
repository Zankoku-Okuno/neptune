module Neptune.Negotiate where

import ClassyPrelude
import Neptune.Core

import Network.HTTP.Media (mapQuality)


negotiate :: Accept a => [(a, b)] -> [Quality a] -> Either [a] (a, b)
negotiate provide accept =
    let provide' = ( \(f, s) -> (f, (f, s)) ) <$> provide -- TODO send in an Issue to http-media
        acceptable = map fst provide
    in case mapQuality provide' accept of
        Nothing -> Left acceptable
        Just result -> Right result

--FIXME I think it'd be more fruitful to allow any `Accept` to have this sort of interface
negotiateMedia :: [(MediaType, b)] -> [Quality MediaType] -> Either Error (MediaType, b)
negotiateMedia provide accept = case negotiate provide accept of
    Left acceptable -> Left $ BadMedia acceptable
    Right result -> Right result

