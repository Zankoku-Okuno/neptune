{-# LANGUAGE OverloadedStrings #-}
import Web.Neptune
import Web.Neptune.Wai

main :: IO ()
main = quickNeptune $ do
    endpoint "home" "GET" "" $ do
        format $ do
            medium "text/plain" $ do
                text "Goodbyte, cruel world!"

