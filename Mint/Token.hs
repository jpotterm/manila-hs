{-# LANGUAGE OverloadedStrings #-}

module Mint.Token (getTokenRequest) where

import Control.Monad.Trans
import qualified Data.ByteString.Lazy.Char8
import Network.HTTP.Conduit
import Network.HTTP.Conduit.Browser
import Text.XML.HXT.Core


getTokenRequest :: BrowserAction String
getTokenRequest = do
    request <- parseUrl "https://wwws.mint.com/overview.event"
    response <- makeRequestLbs request

    let responseString = Data.ByteString.Lazy.Char8.unpack $ responseBody response

    liftIO $ getToken responseString


getToken :: String -> IO String
getToken responseString = do
    let doc = readString [withParseHTML yes, withWarnings no] responseString

    token <- runX $ doc
        //> hasName "input" >>> hasAttrValue "id" (== "javascript-token")
        >>> getAttrValue "value"

    return . head $ token
