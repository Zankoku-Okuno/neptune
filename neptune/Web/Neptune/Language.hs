module Web.Neptune.Language where

import Web.Neptune.Core
import Web.Neptune.Tools.Encoding
import Data.Word8
import Data.Word (Word16)
import qualified Data.ByteString as BS
import Network.HTTP.Media (Quality, Match(..))


instance Match Language where
    matches (Lang ["*"]) (Lang _) = True
    matches (Lang _) (Lang ["*"]) = True
    matches (Lang a) (Lang b) = undefined

    moreSpecificThan (Lang ["*"]) (Lang ["*"]) = False
    moreSpecificThan (Lang ["*"]) (Lang _) = False
    moreSpecificThan (Lang _) (Lang ["*"]) = True
    moreSpecificThan (Lang a) (Lang b) = length a > length b

instance IsString Language where
    fromString "*" = Lang ["*"]
    fromString str = case _parseLang $ fromString str of
        Nothing -> error "string is not a language"
        Just lang -> lang


parseLanguage :: ByteString -> Maybe [Quality Language]
parseLanguage = mapM parseLangParam . BS.split _comma
    where
    parseLangParam :: ByteString -> Maybe (Quality Language)    
    parseLangParam input = case BS.split _semicolon input of
        [lang] -> _addQuality 1000 <$> _parseLang (_trimWs lang)
        [lang, q] -> do
            lang <- _parseLang (_trimWs lang)
            q <- (_trimWs <$>) $ stripPrefix "=" . _trimWs =<< stripPrefix "q" (_trimWs q)
            q <- readQ q
            return $ _addQuality q lang
        _ -> Nothing
_parseLang :: ByteString -> Maybe Language
_parseLang input = case BS.split _hyphen input of 
    [] -> Nothing
    xs -> Lang <$> mapM check xs
    where
    check str | 1 <= BS.length str
                && BS.length str <= 8
                && BS.all isAlpha str = Just str
    check _ = Nothing

showLanguage :: Language -> ByteString
showLanguage (Lang xs) = BS.intercalate "-" xs


-- Stubs waiting for implementation in http-media
_addQuality :: Word16 -> a -> Quality a
_addQuality = undefined --STUB

readQ :: ByteString -> Maybe Word16
readQ = undefined --STUB

-- Missing bits of ByteString interface
stripPrefix :: ByteString -> ByteString -> Maybe ByteString
stripPrefix pre input | pre `BS.isPrefixOf` input = Just $ BS.drop (BS.length pre) input
stripPrefix _ input = Nothing

dropAround :: (Word8 -> Bool) -> ByteString -> ByteString
dropAround p = fst . BS.spanEnd p . BS.dropWhile p

_trimWs :: ByteString -> ByteString
_trimWs = dropAround (`elem` [_space, _tab])
