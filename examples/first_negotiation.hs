{-# LANGUAGE OverloadedStrings #-}
import Web.Neptune
import Web.Neptune.Wai
import qualified Data.ByteString.Lazy as LBS

main :: IO ()
main = quickNeptune $ do
    endpoint "home" "GET" "" $ do
        format $ do
            medium "text/plain" $ do
                text "Goodbyte, cruel world!"
            medium "text/html" $ do
                htmlFile <- liftIO $ LBS.readFile "home.html"
                lbs htmlFile -- use a lazy ByteString for the response body

