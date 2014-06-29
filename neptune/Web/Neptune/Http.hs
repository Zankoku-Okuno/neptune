{-# LANGUAGE OverloadedStrings #-}
module Web.Neptune.Http where

import Web.Neptune.Util
import Web.Neptune.Core

import Data.Word8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import qualified Data.Map as Map

import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as Wai
import qualified Network.HTTP.Types as Wai
import qualified Network.HTTP.Media as Wai
import qualified Web.Cookie as Wai


waiToNeptune :: Wai.Request -> IO Request
waiToNeptune r = do
    (raw_params, raw_files, reqBody) <- parseBody
    return $ Request
        { location = Wai.pathInfo r
        , verb = Wai.requestMethod r
        , acceptType = acceptType
        , acceptLang = error "toNeptune: get acceptLang" --STUB
        , appState = appState
        , parameters = mkMap raw_params
        , attachments = mkMap raw_files
        , reqBody = reqBody
        }
    where
    headers = Wai.requestHeaders r
    acceptType = let accept = fromMaybe "*/*" $ "Accept" `lookup` headers
                 in fromMaybe [] $ Wai.parseAccept accept
    appState = let cookies = maybe [] Wai.parseCookies $ "Cookie" `lookup` headers
               in foldl cookieMap Map.empty cookies
        where cookieMap acc (name, value) = Map.insert (decodeUrl name) value acc
    parseBody = case Wai.getRequestBodyType r of
        Nothing -> do
            let mime = fromMaybe "application/octet-stream" $ do
                it <- "Content-Type" `lookup` headers
                case BS.split _slash . BS.takeWhile (/= _semicolon) $ it of
                    [major,minor] -> Just $ major Wai.// minor
                    _ -> Nothing
            body <- Wai.lazyRequestBody r
            return ([], [], Just (mime, body))
        Just _ -> do
            (params, files) <- Wai.parseRequestBody Wai.lbsBackEnd r
            return (params, files, Nothing)
    mkMap = foldr addParam Map.empty
        where
        addParam (bsName, value) acc =
            let name = decodeUrl bsName
            in if name `Map.member` acc
                then Map.adjust (value:) name acc
                else Map.insert name [value] acc
waiFromNeptune :: ErrorHandlers -> AcceptMedia -> Response -> Wai.Response
waiFromNeptune _ _ r@(Response {}) = Wai.responseLBS Wai.status200 headers (body r)
    where
    headers = mimeHeader ++ langHeader ++ cacheHeader ++ cookies
    mimeHeader = case mimetype r of
        Nothing -> []
        Just mt -> [("Content-Type", fromString . show $ mt)]
    langHeader = [] --STUB
    cacheHeader = case cacheFor r of
        Nothing -> [ ("Cache-Control", "private, max-age=0, no-cache, no-store")]
        Just dt -> [ ("Cache-Control", "no-transform, public, max-age=" <> (fromString . show) dt)
                   , ("Vary", "Accept,Accept-Language,Accept-Encoding") ] --TODO check that this is all varying needed
    cookies = (\(k, v) -> ("Set-Cookie", mkCookie (encodeUrl [61] k) v)) <$> Map.toList (updateAppState r)
        where
        mkCookie name Nothing =
            name <> "=; Max-Age=0"
        mkCookie name (Just (value, Nothing)) =
            name <> "=" <> value
        mkCookie name (Just (value, Just maxage)) =
            name <> "=" <> value <> "; Max-Age=" <> fromString (show maxage)
waiFromNeptune ehs accept (EmptyResponse response text) = undefined --STUB
waiFromNeptune ehs accept (Redirect loc isPerma) = undefined --STUB
waiFromNeptune ehs accept (BadContent allowed) = undefined --STUB
waiFromNeptune ehs accept BadResource = undefined --STUB
waiFromNeptune ehs accept (BadMethod allowed) =
    uncurry (Wai.responseLBS Wai.status405) $ negotiateErrorHeadersAndBody accept headers []
    where
    headers = [("Allowed", BS.intercalate "," allowed)]
    handlers = [(mt, f allowed) | (mt, f) <- ehBadMethod ehs]
waiFromNeptune ehs accept (BadAccept producible) = undefined --STUB
waiFromNeptune ehs accept (BadLanguage) = undefined --STUB
waiFromNeptune ehs accept (NotAuthorized) = undefined --STUB
waiFromNeptune ehs accept (NoUrlReverse eid vault) = undefined --STUB
waiFromNeptune ehs accept (Timeout) = undefined --STUB
waiFromNeptune ehs accept InternalError = undefined --STUB

negotiateErrorHeadersAndBody :: AcceptMedia
                             -> [Wai.Header] -> [(MediaType, LByteString)]
                             -> ([Wai.Header], LByteString)
negotiateErrorHeadersAndBody accept headers formats =
    case negotiate accept formats of
        Nothing -> (headers, "")
        Just (ct, body) -> (("Content-Type", (fromString . show) ct) : headers, body)

serveWai :: Neptune -> Wai.Application
serveWai neptune = app --TODO make sure exceptions get turned into http500
    where
    builtNeptune = buildNeptune "localhost:8080" neptune
    app waiRequest respond = do
        request <- waiToNeptune waiRequest
        let toWai = waiFromNeptune (nErrorHandlers builtNeptune) (acceptType request)
        response <- handleResult toWai $ do
            let routingState = RS { rRequest = request
                                  , rPath = location request
                                  , rParams = Wai.vault waiRequest
                                  }
            m_route <- runRoutesM $ evalHandlers routingState (nHandlers builtNeptune)
            (vault, action) <- case m_route of
                Left [] -> raise BadResource
                Left allowed -> raise $ BadMethod allowed
                Right route -> return route
            let handlingState = HS { hRequest = request
                                   , hParams = vault
                                   , hResponse = def
                                   , hNeptune = builtNeptune
                                   }
            (formats, state) <- runActionM handlingState action
            let acceptable = fst <$> formats
            (mimetype, format) <- maybe (raise $ BadAccept acceptable) return $
                negotiate (acceptType request) formats
            let state' = state { hResponse = (hResponse state) {mimetype = Just mimetype} }
            body <- runFormatM state' format
            return . toWai $ (hResponse state') { body = body }
        respond response
    handleResult :: (Response -> Wai.Response) -> ResultT IO Wai.Response -> IO Wai.Response
    handleResult toWai x = runResultT x >>= \res -> case res of
        Normal x' -> return x'
        Alternate response -> return $ toWai response


