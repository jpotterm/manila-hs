{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Mint.Account (mintAccounts) where

import Control.Lens
import Control.Monad.Trans
import Data.Aeson
import Data.Aeson.Lens (key)
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import Database.HDBC
import Database.HDBC.Sqlite3
import GHC.Generics
import Network.URI
import qualified Network.Wreq as Wreq

import qualified Settings
import Util


data JsonAccount = JsonAccount { name     :: String
                               , value    :: Float
                               , isActive :: Bool
                               } deriving (Show, Generic)
instance FromJSON JsonAccount


mintAccounts :: Wreq.Options -> IO ()
mintAccounts tokenSession = do
    let accountUrl = Settings.mintHostname ++ "/bundledServiceController.xevent?legacy=false"
    let accountJson = "[{"
                    ++ "\"id\": \"1\","
                    ++ "\"service\": \"MintAccountService\","
                    ++ "\"task\": \"getAccountsSortedByBalanceDescending\","
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
    response <- Wreq.postWith tokenSession accountUrl ["input" Wreq.:= BS.pack accountJson]

    let resultMaybe = response ^? Wreq.responseBody . key "response" . key "1" . key "response"
    case fmap fromJSON resultMaybe :: Maybe (Result [JsonAccount]) of
         Just (Success jsonAccounts) -> updateAccounts $ filter isActive jsonAccounts
         Just (Error message)        -> putStrLn ("Error: could not decode account JSON response. " ++ message)
         Nothing                     -> putStrLn "Error: could not decode account JSON response."


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
