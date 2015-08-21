{-| This module contains functions for building URLs. In particular, use 'simpleURL'
    to begin, and add additional components with the other functions of this module.

    There are no functions to remove parts of URLs. The idea is that you either have
    a URL that you want to read from (and therefore need not modify) or you are
    creating a URL, in which case one should only need add additional information.
-}
module Web.Neptune.Core.Url (
    -- * URL Types
      URL
    , Scheme
    , Host
    , PathInfo
    -- * Building URLs
    , simpleUrl
    , urlUser
    , urlPort
    , urlPath
    , urlQuery
    , urlHash
    ) where

import Web.Neptune.Core.Types
import Web.Neptune.Core.Util
import qualified Data.Map as Map

--http://tools.ietf.org/html/rfc3986#section-3
--lowercase is cannonical

{-| Build a URL with only a scheme, host and path. This is often
    sufficient, but more complex URLs can be built from simple ones
    using the other functions in this module.

    In particular, there is no user, port, query parameters or fragment
    specified in the output of this function.
-}
simpleUrl :: Scheme -> Host -> PathInfo -> URL
simpleUrl scheme host path = URL
    { _scheme = scheme
    , _user = Nothing
    , _host = host
    , _port = Nothing
    , _path = path
    , _query = Map.empty
    , _fragment = Nothing
    }

-- |Set the user for a URL.
urlUser :: URL -> ByteString -> URL
urlUser url user' = url { _user = Just user' }

-- |Set the port for a URL.
urlPort :: URL -> Int -> URL
urlPort url port' = url { _port = Just port' }

-- |Append to the existing path of a URL.
urlPath :: URL -> [Text] -> URL
urlPath url path' = url { _path = _path url ++ path' }

-- |Append a query name and parameter value.
--  Setting a key more than once is acceptable.
urlQuery :: URL -> (Text, Parameter) -> URL
urlQuery url (k,v) = case k `Map.lookup` qs of
    Nothing -> url { _query = Map.insert k [v] qs }
    Just vs -> url { _query = Map.insert k (vs++[v]) qs }
    where qs = _query url

-- |Set the URL fragment.
urlHash :: URL -> Text -> URL
urlHash url hash' = url { _fragment = Just hash' }