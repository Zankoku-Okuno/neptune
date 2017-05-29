module Neptune.Route where

import ClassyPrelude
import Neptune.Core

type Route = Location -> Maybe NeptuneApp

dispatch :: [Route] -> Location -> Maybe NeptuneApp
dispatch rs rid = listToMaybe . catMaybes $ ($ rid) <$> rs