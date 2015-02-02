{-| Serve Neptune applications over Wai. -}
module Web.Neptune.Wai (
      serveWai
    , quickNeptune
    , waiToNeptune
    , waiFromNeptune
    ) where

import Web.Neptune
import Web.Neptune.Tools

import Data.Word8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import qualified Data.Map as Map
import qualified Data.Vault.Lazy as Vault

import qualified Network.HTTP.Media as Web

import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as Wai
import qualified Network.HTTP.Types as Wai
import qualified Web.Cookie as Wai
import qualified Network.Wai.Handler.Warp as Warp


-- |Wrap a compiled Neptune application into a Wai application.
serveWai :: NeptuneServer -> Wai.Application
serveWai neptune = waiApp
    where
    neptuneApp = serve neptune
    toWai = waiFromNeptune (nErrorHandlers neptune)
    waiApp waiRequest respond = do
        incoming <- waiToNeptune waiRequest
        response <- neptuneApp incoming
        let waiResponse = toWai (acceptType incoming) response
        respond waiResponse

-- |Set up a development server on @http:\/\/localhost:8080@ running a Neptune application.
quickNeptune :: Neptune -> IO ()
quickNeptune neptune = do
    putStrLn "Running Neptune (http://localhost:8080)..."
    putStrLn "(Ctrl-C to quit)"
    let prepath = simpleUrl "http" "localhost" [] `urlPort` 8080
        app = compileNeptune Vault.empty prepath neptune
    Warp.run 8080 . serveWai $ app


-- |Transform a Wai request into a Neptune request.
waiToNeptune :: Wai.Request -> IO Request
waiToNeptune r = do
    (raw_query, raw_files, raw_body) <- the_body
    return $ Request
        { resource = normalizePath $ Wai.pathInfo r
        , verb = Wai.requestMethod r
        , acceptType = the_acceptType
        , acceptLang = the_acceptLang
        , appState = the_appState
        , queries = mkMap raw_query
        , attachments = mkMap raw_files
        , requestBody = raw_body
        , requestData = Wai.vault r
        }
    where
    headers = Wai.requestHeaders r
    the_acceptType = let accept = fromMaybe "*/*" $ "Accept" `lookup` headers
                 in fromMaybe [] $ Web.parseQuality accept
    the_acceptLang = let accept = fromMaybe "*" $ "Accept-Language" `lookup` headers
                 in fromMaybe [] $ Web.parseQuality accept
    the_appState = let cookies = maybe [] Wai.parseCookies $ "Cookie" `lookup` headers
               in foldl cookieMap Map.empty cookies
        where cookieMap acc (name, value) = Map.insert (decodePercent name) value acc
    the_body = case Wai.getRequestBodyType r of
        Nothing -> do
            let mime = fromMaybe "application/octet-stream" $ do
                it <- "Content-Type" `lookup` headers
                case BS.split _slash . BS.takeWhile (/= _semicolon) $ it of
                    [major,minor] -> Just $ major Web.// minor
                    _ -> Nothing
            raw_body <- Wai.lazyRequestBody r
            return ([], [], Just (mime, raw_body))
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


-- |Transform a Neptune response into a Wai response.
waiFromNeptune :: ErrorHandlers -> AcceptMedia -> Response -> Wai.Response
waiFromNeptune _ _ r@(Response {}) = case body r of
        LBSResponse rendered -> Wai.responseLBS Wai.status200 headers rendered
        BuilderResponse rendered -> Wai.responseBuilder Wai.status200 headers rendered
        FileResponse filepath -> Wai.responseFile Wai.status200 headers filepath Nothing
    where
    headers = mimeHeader ++ langHeader ++ cacheHeader ++ cookies
    mimeHeader = case contentType r of
        Nothing -> []
        Just mt -> [("Content-Type", fromString . show $ mt)]
    langHeader = case language r of
        Nothing -> []
        Just lang -> [("Content-Language", fromString . show $ lang)]
    cacheHeader = case cacheFor r of
        Nothing -> [ ("Cache-Control", "private, max-age=0, no-cache, no-store")]
        Just dt -> [ ("Cache-Control", "no-transform, public, max-age=" <> (fromString . show) dt)
                   , ("Vary", "Accept,Accept-Language,Accept-Encoding,Cookie") ] --TODO check that this is all varying needed
    cookies = (\(k, v) -> ("Set-Cookie", mkCookie (encodePercent k) v)) <$> Map.toList (updateAppState r)
        where
        --FIXME let me get at the config here to set up the "Secure" attribute when running over TLS
        mkCookie name Nothing =
            name <> "=; Max-Age=0"
        mkCookie name (Just (value, Nothing)) =
            name <> "=" <> value
        mkCookie name (Just (value, Just maxage)) =
            name <> "=" <> value <> "; Max-Age=" <> fromString (show maxage)
waiFromNeptune ehs accept (CustomResponse cause _) =
    waiFromNeptune ehs accept (InternalError $ "Error: Cannot send response type: " <> cause)
waiFromNeptune _ _ (Redirect reason loc) = Wai.responseLBS status headers ""
    where
    headers = [("Location", showURL loc)]
    status = case reason of
        Created -> Wai.status201
        Moved -> Wai.status301
        Temporary -> Wai.status307

waiFromNeptune ehs accept (BadContent allowed) = Wai.responseLBS Wai.status415 headers (f allowed)
    where (headers, f) = negotiateError (const "") accept
                           [("Allowed", BS.intercalate "," (fromString . show <$> allowed))]
                           (ehBadContent ehs)
waiFromNeptune ehs accept BadResource = Wai.responseLBS Wai.status404 headers rendered
    where (headers, rendered) = negotiateError "" accept [] (ehBadResource ehs)
waiFromNeptune ehs accept (BadVerb allowed) = Wai.responseLBS Wai.status405 headers (f allowed)
    where (headers, f) = negotiateError (const "") accept
                           [("Allowed", BS.intercalate "," allowed)]
                           (ehBadVerb ehs)
waiFromNeptune ehs accept (BadAccept producible) = Wai.responseLBS Wai.status406 headers (f producible)
    where (headers, f) = negotiateError (const "") accept
                           [("Allowed", BS.intercalate "," (fromString . show <$> producible))]
                           (ehBadAccept ehs)
--FIXME this should return a list of supported languages
waiFromNeptune ehs accept BadLanguage = Wai.responseLBS Wai.status406 headers rendered
    where (headers, rendered) = negotiateError "" accept [] (ehBadLanguage ehs)
waiFromNeptune ehs accept BadPermissions = Wai.responseLBS Wai.status403 headers rendered
    where (headers, rendered) = negotiateError "" accept [] (ehBadPermissions ehs)
waiFromNeptune ehs accept (Timeout dt) = Wai.responseLBS Wai.status504 headers (f dt)
    where (headers, f) = negotiateError (const "") accept [] (ehTimeout ehs)
waiFromNeptune ehs accept (InternalError msg) = Wai.responseLBS Wai.status500 headers (f msg)
    where (headers, f) = negotiateError (LBS.fromStrict . encodeUtf8) accept [] (ehInternalError ehs)

negotiateError :: a -> AcceptMedia
               -> [Wai.Header] -> [(MediaType, a)]
               -> ([Wai.Header], a)
negotiateError emptyBody accept headers formats =
    case negotiate accept formats of
        Nothing -> (headers, emptyBody)
        Just (ct, res) -> (("Content-Type", (fromString . show) ct) : headers, res)

