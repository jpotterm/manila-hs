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


mintLogin :: String -> String -> IO (Wreq.Options, Wreq.Options)
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
                    , "browser" Wreq.:= Text.pack "firefox"
                    , "browserVersion" Wreq.:= Text.pack "27"
                    , "os" Wreq.:= Text.pack "linux"
                    ]
    let loginOpts = Wreq.defaults & Wreq.cookies .~ preLoginCookieJar & Wreq.header "accept" .~ [(TE.encodeUtf8 "application/json")]
    login <- Wreq.postWith loginOpts loginUrl loginBody
    let loginCookieJar = login ^. Wreq.responseCookieJar

    return $ mintOptions loginCookieJar $ getToken login


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
