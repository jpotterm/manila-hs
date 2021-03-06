{-# LANGUAGE OverloadedStrings #-}

module Mint.Login (mintLogin) where

import qualified Data.Aeson as Aeson
import Data.Aeson.Lens (key)
import Control.Lens
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client (CookieJar)
import qualified Network.Wreq as Wreq

import qualified Settings


mintLogin :: String -> String -> IO (Maybe (Wreq.Options, Wreq.Options))
mintLogin username password = do
    let preLoginUrl = Settings.mintHostname ++ "/login.event?task=L"
    let loginUrl = Settings.mintHostname ++ "/loginUserSubmit.xevent"

    -- Pre-login. For some reason Mint.com requires you to visit the login page
    -- before you post login credentials. If this step is left out then login
    -- will fail intermittently.
    preLogin <- Wreq.get loginUrl
    let preLoginCookieJar = preLogin ^. Wreq.responseCookieJar

    -- Login
    let loginBody = [ "username" Wreq.:= Text.pack username
                    , "password" Wreq.:= Text.pack password
                    , "task" Wreq.:= Text.pack "L"
                    ]
    let loginOpts = Wreq.defaults & Wreq.cookies .~ preLoginCookieJar & Wreq.header "accept" .~ [(TE.encodeUtf8 "application/json")]
    login <- Wreq.postWith loginOpts loginUrl loginBody

    let (loginSuccess, errorMessage) = checkLoginSuccess login
    if loginSuccess
        then return $ Just $ mintOptions (login ^. Wreq.responseCookieJar) (getToken login)
        else do
            putStrLn $ "Mint.com login failed: " ++ errorMessage
            return Nothing


checkLoginSuccess :: Wreq.Response LBS.ByteString -> (Bool, String)
checkLoginSuccess response =
    let maybeErrorMessage = response ^? Wreq.responseBody . key "error" . key "vError" . key "copy"
    in  case maybeErrorMessage of
             Just (Aeson.String errorMessage) -> (False, Text.unpack errorMessage)
             otherwise                        -> (True, "")


getToken :: Wreq.Response LBS.ByteString -> String
getToken response =
    let tokenValue = response ^? Wreq.responseBody . key "sUser" . key "token"
    in  case tokenValue of
             Just (Aeson.String s) -> Text.unpack s
             otherwise             -> ""


mintOptions :: CookieJar -> String -> (Wreq.Options, Wreq.Options)
mintOptions loginCookieJar token =
    let session = Wreq.defaults & Wreq.cookies .~ loginCookieJar
        tokenSession = session & Wreq.header "token" .~ [(TE.encodeUtf8 $ Text.pack token)]
    in  (session, tokenSession)
