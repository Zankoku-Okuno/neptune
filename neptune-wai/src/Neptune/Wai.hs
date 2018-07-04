{-#LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns #-}
module Neptune.Wai where

import ClassyPrelude
import Neptune

import Data.String
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Network.HTTP.Types as Http
import Network.HTTP.Media (parseQuality, renderHeader)
import qualified Network.Wai as Wai

-- import qualified Data.Map as Map


toWaiApp :: NeptuneApp -> Wai.Application
toWaiApp app waiReq respond = do
    req <- fromWaiReq waiReq
    response <- app req
    respond $ toWaiResponse response


fromWaiQuery :: Http.Query -> Query
fromWaiQuery [] = mempty
fromWaiQuery ((k, fromMaybe "" -> v) : qs) = insertWith (flip (++)) k [v] (fromWaiQuery qs)


fromWaiReq :: Wai.Request -> IO Request
fromWaiReq req = do
    content <- case lookup "Content-Type" (Wai.requestHeaders req) of
        Nothing -> pure Nothing
        Just typeStr -> do
            let bodyType = fromString . T.unpack . T.decodeUtf8 $ typeStr -- FIXME is this really the best way to get the content type?
            bodyContent <- LBS.fromStrict <$> Wai.requestBody req
            pure $ Just (bodyType, bodyContent)
    pure Request
        { resourceId = (Wai.pathInfo req, fromWaiQuery $ Wai.queryString req)
        , method = Wai.requestMethod req
        , requestBody = content
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