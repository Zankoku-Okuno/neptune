module Web.Neptune.Core.Url where

import Web.Neptune.Core.Types
import Web.Neptune.Core.Util
import qualified Data.Map as Map

--http://tools.ietf.org/html/rfc3986#section-3
--lowercase is cannonical

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

urlUser :: URL -> ByteString -> URL
urlUser url user' = url { _user = Just user' }

urlPort :: URL -> Int -> URL
urlPort url port' = url { _port = Just port' }

urlPath :: URL -> [Text] -> URL
urlPath url path' = url { _path = _path url ++ path' }

urlQuery :: URL -> (Text, Parameter) -> URL
urlQuery url (k,v) = case k `Map.lookup` qs of
	Nothing -> url { _query = Map.insert k [v] qs }
	Just vs -> url { _query = Map.insert k (vs++[v]) qs }
	where qs = _query url

urlHash :: URL -> Text -> URL
urlHash url hash' = url { _fragment = Just hash' }