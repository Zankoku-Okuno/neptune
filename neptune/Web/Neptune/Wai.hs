{-# LANGUAGE OverloadedStrings #-}
module Web.Neptune.Wai where

import Web.Neptune
import Web.Neptune.Core

import Numeric (showHex)
import Data.Word8

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Text.Encoding

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
quickNeptune neptune = do
    putStrLn "Running Neptune..."
    putStrLn "(Ctrl-C to quit)"
    Warp.run 8080 . serveWai . buildNeptune "http://localhost:8080" Vault.empty $ neptune





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






{-| Utf-8 encode and %-escape all control characters, space, percent, and high-order bytes.
    
    Additionally, any bytes passed in the '[Word8]' argument are also percent-escaped.
    This can be useful, for example, to escape slash, question mark and ampersand in URIs.
-}
encodePercent :: [Word8] -> Text -> ByteString
encodePercent extra = BS.pack . concatMap (encodePercentByte extra) . BS.unpack . encodeUtf8

encodePercentByte :: [Word8] -> Word8 -> [Word8]
encodePercentByte extra c | c <= 32 || c >= 127 
                     || c == _percent
                     || c `elem` extra
    = (_percent:) . BS.unpack . fromString . toHex $ c
    where
    toHex c = if length str == 1 then '0':str else str
        where str = showHex c ""
encodePercentByte _ c = [c]

{-| Convert %-escapes into bytes and Utf-8 decode.

    Plus signs are not converted.
-}
decodePercent :: ByteString -> Text
decodePercent = decodeUtf8 . BS.pack . reverse . go [] . BS.unpack
    where
    go :: [Word8] -> [Word8] -> [Word8]
    go acc [] = acc
    go acc (_percent:b:l:rest) = case (fromHex b, fromHex l) of
        (Just b', Just l') -> go (fromIntegral (16*b' + l') : acc) rest
        _ -> go (_percent:acc) (b:l:rest)
    go acc (c:rest) = go (c:acc) rest
    fromHex :: Word8 -> Maybe Int
    fromHex 0x30 = Just 0
    fromHex 0x31 = Just 1
    fromHex 0x32 = Just 2
    fromHex 0x33 = Just 3
    fromHex 0x34 = Just 4
    fromHex 0x35 = Just 5
    fromHex 0x36 = Just 6
    fromHex 0x37 = Just 7
    fromHex 0x38 = Just 8
    fromHex 0x39 = Just 9
    fromHex 0x61 = Just 10
    fromHex 0x62 = Just 11
    fromHex 0x63 = Just 12
    fromHex 0x64 = Just 13
    fromHex 0x65 = Just 14
    fromHex 0x66 = Just 15
    fromHex 0x41 = Just 10
    fromHex 0x42 = Just 11
    fromHex 0x43 = Just 12
    fromHex 0x44 = Just 13
    fromHex 0x45 = Just 14
    fromHex 0x46 = Just 15
    fromHex _ = Nothing
    
