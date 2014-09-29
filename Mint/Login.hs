{-# LANGUAGE OverloadedStrings #-}

module Mint.Login (mintLogin) where

import qualified Data.Aeson as Aeson
import Data.Aeson.Lens (key)
import Control.Lens
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as Text
import Network.HTTP.Client (CookieJar)
import qualified Network.Wreq as Wreq

import qualified Settings


mintLogin :: String -> String -> IO (Wreq.Options, Wreq.Options)
mintLogin username password = do
    let preLoginUrl = Settings.mintHostname ++ "/login.event?task=L"
    let loginUrl = Settings.mintHostname ++ "/loginUserSubmit.xevent"

    -- Pre-login
    preLogin <- Wreq.get loginUrl
    let preLoginCookieJar = preLogin ^. Wreq.responseCookieJar

    -- Login
    let loginBody = [ "username" Wreq.:= BS.pack username
                    , "password" Wreq.:= BS.pack password
                    , "task" Wreq.:= BS.pack "L"
                    , "browser" Wreq.:= BS.pack "firefox"
                    , "browserVersion" Wreq.:= BS.pack "27"
                    , "os" Wreq.:= BS.pack "linux"
                    ]
    let loginOpts = Wreq.defaults & Wreq.cookies .~ preLoginCookieJar & Wreq.header "accept" .~ [(BS.pack "application/json")]
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
        tokenSession = session & Wreq.header "token" .~ [(BS.pack token)]
    in  (session, tokenSession)
