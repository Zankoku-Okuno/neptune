{-# LANGUAGE OverloadedStrings #-}
import Web.Neptune
import Web.Neptune.Wai
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = quickNeptune $ do
    endpoint "home" "GET" "" $ do
        format $ do
            medium "text/plain" $ do
                text "Goodbyte, cruel world!"
            -- Just showing off how common code can be extracted
            -- The same thing can be done with Action, Neptune, and even Route
            msgHtml "Neptuneから今日は"
    -- Notice the ":msg" path segment in this route. This captures that segment as a QDatum
    endpoint "msg" "GET" "echo/:msg" $ do
        -- Here, we use qDatum_ff to get the part of the URL captured by ":msg"
        -- The two 'f's are a double-force:
        --   it must be present or error out (no danger of that here)
        --   it must parse to the correct type (we only need Text, so again no danger here)
        msg <- qDatum_ff "msg"
        format $ msgHtml msg
    endpoint "static" "GET" "static/..." $ do
        setCache $ 24 * 60 * 60
        path <- T.unpack . T.intercalate "/" <$> datum_f pathKey
        anyFormat $ sendfile ("static/" ++ path)

msgHtml :: Text -> Formats
msgHtml msg = medium "text/html" $ do
    htmlFile <- liftIO $ T.readFile "home.html"
    let (pre, rest) = T.breakOn slot htmlFile
        post        = T.drop (T.length slot) rest
    text $ pre <> msg <> post
    where
    slot = "{{msg}}" :: Text

