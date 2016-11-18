{-#LANGUAGE TypeOperators,
            ExistentialQuantification #-}
module Web.Neptune.Util.Dispatch
    ( (->>), match, cases
    , matches, branch
    , module Web.Neptune.Util.Partial
    ) where

import Prelude hiding (id, (.))

import Data.Maybe
import Web.Neptune.Util.Partial

data a ->> b = forall vars. Branch (a ->? vars) (vars -> b)

infix 0 ->>
(->>) :: (a ->? vars) -> (vars -> b) -> (a ->> b)
(->>) = Branch

-- take the first branch which can be taken
match :: a -> [a ->> b] -> Maybe b
match val branches = listToMaybe $ val `matches` branches

-- flip match
cases = flip match

-- take all branches (lazy, so if you only deconstruct the first in this list, only the first branch is calculated)
matches :: a -> [a ->> b] -> [b]
matches val branches = catMaybes $ branch val <$> branches

-- attempt to take a single branch
branch :: a -> (a ->> b) -> Maybe b
branch val (Branch pat body) = body <$> (pat $? val)

