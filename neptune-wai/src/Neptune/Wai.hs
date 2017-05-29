{-#LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns #-}
module Neptune.Wai where

import ClassyPrelude
import Neptune

import qualified Network.HTTP.Types as Http
import Network.HTTP.Media (parseQuality, renderHeader)
import qualified Network.Wai as Wai

-- import qualified Data.Map as Map


toWaiApp :: NeptuneApp -> Wai.Application
toWaiApp app waiReq respond = do
    let req = fromWaiReq waiReq
    response <- app req
    respond $ toWaiResponse response


fromWaiQuery :: Http.Query -> Query
fromWaiQuery [] = mempty
fromWaiQuery ((k, fromMaybe "" -> v) : qs) = insertWith (flip (++)) k [v] (fromWaiQuery qs)


fromWaiReq :: Wai.Request -> Request
fromWaiReq req = Request
    { resourceId = (Wai.pathInfo req, fromWaiQuery $ Wai.queryString req)
    , method = Wai.requestMethod req
    , negotiation = Negotiation
        { acceptMedia = fromMaybe (fromJust $ parseQuality "*/*") $
                            parseQuality =<< lookup "Accept" (Wai.requestHeaders req)
        }
    }


toWaiResponse :: Response -> Wai.Response
toWaiResponse Response{ responseBody = Nothing, .. } = Wai.responseLBS status [] ""
toWaiResponse Response{ responseBody = Just responseBody, .. } = Wai.responseLBS status headers (snd responseBody)
    where
    headers = [("Content-Type", renderHeader (fst responseBody))]



fromJust (Just x) = x
fromJust Nothing = error "fromJust of Nothing"