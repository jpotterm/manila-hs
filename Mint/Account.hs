{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Mint.Account (getAccountsRequest) where

import Control.Monad.Trans
import Data.Aeson
import qualified Data.ByteString.Char8
import qualified Data.ByteString.Lazy.Char8
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as HM
import Database.HDBC
import Database.HDBC.Sqlite3
import GHC.Generics
import Network.HTTP.Conduit
import Network.HTTP.Conduit.Browser
import Network.URI

import Util


data JsonAccount = JsonAccount { name           :: String
                               , value :: Float
                               , isActive       :: Bool
                               } deriving (Show, Generic)
instance FromJSON JsonAccount


getAccountsRequest :: String -> BrowserAction (Response L.ByteString)
getAccountsRequest token = do
    let encodedToken = escapeURIString isUnescapedInURI token
    requestUrl <- parseUrl $ "https://wwws.mint.com/bundledServiceController.xevent?token=" ++ encodedToken
    let requestMethod = requestUrl {method = "POST"}
    let input = "[{"
                    ++ "\"id\": \"1\","
                    ++ "\"service\": \"MintAccountService\","
                    ++ "\"task\": \"getAccountsSorted\","
                    ++ "\"args\": {"
                        ++ "\"types\": ["
                            ++ "\"BANK\","
                            ++ "\"CREDIT\","
                            ++ "\"INVESTMENT\","
                            ++ "\"LOAN\","
                            ++ "\"MORTGAGE\","
                            ++ "\"OTHER_PROPERTY\","
                            ++ "\"REAL_ESTATE\","
                            ++ "\"VEHICLE\","
                            ++ "\"UNCLASSIFIED\""
                        ++ "]"
                    ++ "}"
                ++ "}]"

    let request = urlEncodedBody [("input", Data.ByteString.Char8.pack input)] requestMethod
    response <- makeRequestLbs request
    let result = decode (responseBody response) :: Maybe Value

    case result of
        Just (Object o) -> do
            let Just (Object o1) = HM.lookup "response" o
            let Just (Object o2) = HM.lookup "1" o1
            let Just o3 = HM.lookup "response" o2

            case fromJSON o3 :: Result [JsonAccount] of
                Error message -> liftIO $ putStrLn ("fromJSON error: " ++ message)
                Success jsonAccounts -> liftIO . updateAccounts $ filter isActive jsonAccounts
        _ -> liftIO $ putStrLn "Decode error"

    return response


updateAccounts :: [JsonAccount] -> IO ()
updateAccounts jsonAccounts = do
    conn <- getDbConnection

    run conn "DELETE FROM account" []
    commit conn

    mapM_ (insertAccount conn) jsonAccounts

    commit conn
    disconnect conn


insertAccount :: Connection -> JsonAccount -> IO Integer
insertAccount conn jsonAccount = do
    let balance = (truncate $ value jsonAccount * 100) :: Integer
    let accountName = (toSql . name) jsonAccount

    run conn "INSERT INTO account (balance, name) VALUES (?, ?)" [toSql balance, toSql accountName]
