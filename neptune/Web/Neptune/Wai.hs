{-# LANGUAGE OverloadedStrings #-}
module Web.Neptune.Wai where

import Web.Neptune.Util
import Web.Neptune.Core

import Data.Word8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import qualified Data.Map as Map
import qualified Data.Vault.Lazy as Vault

import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as Wai
import qualified Network.HTTP.Types as Wai
import qualified Network.HTTP.Media as Wai
import qualified Web.Cookie as Wai
import qualified Network.Wai.Handler.Warp as Warp


serveWai :: NeptuneState -> Wai.Application
serveWai neptune = waiApp
    where
    neptuneApp = serve neptune
    toWai = waiFromNeptune (nErrorHandlers neptune)
    waiApp waiRequest respond = do
        request <- waiToNeptune waiRequest
        response <- neptuneApp request
        let waiResponse = toWai (acceptType request) response
        respond waiResponse

quickNeptune :: Neptune -> IO ()
quickNeptune = Warp.run 8080 . serveWai . buildNeptune "http://localhost:8080" Vault.empty

formatURI :: Location -> ByteString
formatURI (domain, path, query) = encode domain <> "/" <> raw_path <> raw_query
    where
    encode = encodePercent [_slash, _question, _ampersand]
    raw_path = BS.intercalate "/" (encode <$> path)
    raw_query = case Map.toList query of
        [] -> ""
        query -> undefined --STUB



waiToNeptune :: Wai.Request -> IO Request
waiToNeptune r = do
    (raw_query, raw_files, body) <- parseBody
    return $ Request
        { resource = Wai.pathInfo r
        , verb = Wai.requestMethod r
        , acceptType = acceptType
        , acceptLang = error "toNeptune: get acceptLang" --STUB
        , appState = appState
        , queries = mkMap raw_query
        , attachments = mkMap raw_files
        , requestBody = body
        , requestData = Wai.vault r
        }
    where
    headers = Wai.requestHeaders r
    acceptType = let accept = fromMaybe "*/*" $ "Accept" `lookup` headers
                 in fromMaybe [] $ Wai.parseAccept accept
    appState = let cookies = maybe [] Wai.parseCookies $ "Cookie" `lookup` headers
               in foldl cookieMap Map.empty cookies
        where cookieMap acc (name, value) = Map.insert (decodePercent name) value acc
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
            let name = decodePercent bsName
            in if name `Map.member` acc
                then Map.adjust (value:) name acc
                else Map.insert name [value] acc


waiFromNeptune :: ErrorHandlers -> AcceptMedia -> Response -> Wai.Response
waiFromNeptune _ _ r@(Response {}) = case body r of
        LBSResponse body -> Wai.responseLBS Wai.status200 headers body
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
    cookies = (\(k, v) -> ("Set-Cookie", mkCookie (encodePercent [61] k) v)) <$> Map.toList (updateAppState r)
        where
        mkCookie name Nothing =
            name <> "=; Max-Age=0; HttpOnly"
        mkCookie name (Just (value, Nothing)) =
            name <> "=" <> value
        mkCookie name (Just (value, Just maxage)) =
            name <> "=" <> value <> "; Max-Age=" <> fromString (show maxage) <> "; HttpOnly"
waiFromNeptune ehs accept (EmptyResponse response text) = undefined --STUB
waiFromNeptune ehs accept (Redirect loc True) = Wai.responseLBS Wai.status301 headers ""
    where headers = [("Location", formatURI loc)]
waiFromNeptune ehs accept (Redirect loc False) = Wai.responseLBS Wai.status307 headers ""
    where headers = [("Location", formatURI loc)]


waiFromNeptune ehs accept (BadContent allowed) = Wai.responseLBS Wai.status415 headers (f allowed)
    where (headers, f) = negotiateError (const "") accept
                           [("Allowed", BS.intercalate "," (fromString . show <$> allowed))]
                           (ehBadContent ehs)
waiFromNeptune ehs accept BadResource = Wai.responseLBS Wai.status404 headers body
    where (headers, body) = negotiateError "" accept [] (ehBadResource ehs)
waiFromNeptune ehs accept (BadVerb allowed) = Wai.responseLBS Wai.status405 headers (f allowed)
    where (headers, f) = negotiateError (const "") accept
                           [("Allowed", BS.intercalate "," allowed)]
                           (ehBadVerb ehs)
waiFromNeptune ehs accept (BadAccept producible) = Wai.responseLBS Wai.status406 headers (f producible)
    where (headers, f) = negotiateError (const "") accept
                           [("Allowed", BS.intercalate "," (fromString . show <$> producible))]
                           (ehBadAccept ehs)
waiFromNeptune ehs accept (BadLanguage) = error "no BadLanguage handler" --STUB
waiFromNeptune ehs accept BadPermissions = Wai.responseLBS Wai.status403 headers body
    where (headers, body) = negotiateError "" accept [] (ehBadPermissions ehs)
waiFromNeptune ehs accept (Timeout dt) = Wai.responseLBS Wai.status504 headers (f dt)
    where (headers, f) = negotiateError (const "") accept [] (ehTimeout ehs)
waiFromNeptune ehs accept InternalError = Wai.responseLBS Wai.status500 headers body
    where (headers, body) = negotiateError "" accept [] (ehInternalError ehs)
waiFromNeptune ehs accept (NoUrlReverse eid vault) = Wai.responseLBS Wai.status500 headers (f eid vault)
    where (headers, f) = negotiateError (const $ const "") accept [] (ehNoUrlReverse ehs)

negotiateError :: a -> AcceptMedia
               -> [Wai.Header] -> [(MediaType, a)]
               -> ([Wai.Header], a)
negotiateError empty accept headers formats =
    case negotiate accept formats of
        Nothing -> (headers, empty)
        Just (ct, body) -> (("Content-Type", (fromString . show) ct) : headers, body)

