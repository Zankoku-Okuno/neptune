{-# LANGUAGE OverloadedStrings #-}
import Web.Neptune
import Web.Neptune.Wai
import qualified Data.Text.IO as T
import qualified Data.Vault.Lazy as Vault

main :: IO ()
main = quickNeptune $ do
    let fname = "upload/name"
    endpoint "name" "GET" "" $ do
        format $ do
            plaintext $ sendfile fname
            json $ do
                contents <- liftIO $ T.readFile fname
                text $ "{'name':\"" <> contents <> "\"}"
    endpoint "name" "PUT" "" $ do
        body <- parseBody [("text/plain", return . toStrictT . decodeUtf8L)]
        liftIO $ T.writeFile fname body
        redirect =<< url "name" Vault.empty []

plaintext :: Format -> Formats
plaintext = medium "text/plain"

json :: Format -> Formats
json = medium "application/json"

