{-# LANGUAGE OverloadedStrings #-}
import Web.Neptune
import Web.Neptune.Wai

main = quickNeptune $ do
    endpoint "root" "GET" ":gerund,\0" $ do
        gerund <- qDatumOr_f "risin'" "gerund"
        format $ do
            medium "text/plain" $
                text $ "Valleys of Neptune is " <> gerund <> "..."
            medium "text/html" $
                text $ "<p>Valleys of Neptune is " <> gerund <> "...</p>"
    endpoint "file" "GET" "file/..."  $ do
        path <- mconcat . map ("/" <>) <$> datum_f pathKey
        format $ do
            medium "text/plain" $ text $ "Requested path: " <> path