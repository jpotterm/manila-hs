{-# LANGUAGE OverloadedStrings #-}

module Command.Mint (pullCommand, mintCommand) where

import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Data.UUID
import Database.HDBC
import Database.HDBC.Sqlite3
import Network.HTTP.Conduit
import Network.HTTP.Conduit.Browser
import Network.URI
import System.Directory
import System.Posix.User
import System.Process
import qualified Data.ByteString.Char8 as BS

import Mint.Account
import Mint.Category
import Mint.Token
import Mint.Transaction
import Utils


mintCommand :: [String] -> String -> IO ()
mintCommand args flags = do
    conn <- getDbConnection
    instanceUUID <- getInstanceUUID conn

    username <- prompt "Mint.com username: "
    password <- prompt "Mint.com password: "

    let arguments = [ "add-generic-password"
                    , "-a", username
                    , "-s", "manila:" ++ toString instanceUUID
                    , "-w", password
                    , "-U"
                    ]

    (exitCode, output, _) <- readProcessWithExitCode "security" arguments []
    disconnect conn


pullCommand :: [String] -> String -> IO ()
pullCommand args flags = do
    username <- prompt "Mint.com username: "
    password <- promptPassword "Mint.com password: "

    deleteTransactions
    scrape username password
    importTransactions
    removeFile "project/transactions.csv"


scrape :: String -> String -> IO ()
scrape username password = do
    let hostname = "https://wwws.mint.com"

    login <- parseUrl $ hostname ++ "/loginUserSubmit.xevent"
    let loginReq = login {method = "POST"}
    let loginParam = urlEncodedBody
                        [("username", BS.pack username),
                         ("password", BS.pack password),
                         ("task", "L")]
                        loginReq

    transactions <- parseUrl $ hostname ++ "/transactionDownload.event"

    man <- newManager def
    _ <- runResourceT $ browse man $ do
        makeRequestLbs loginParam
        token <- getTokenRequest
        getAccountsRequest token
        getCategoriesRequest
        downloadFile "project/transactions.csv" transactions

    return ()


retrievePassword :: IO ()
retrievePassword = do
    loginName <- getLoginName

    -- Use instance id for the following service name
    (exitCode, output, _) <- readProcessWithExitCode "security" ["find-generic-password", "-a", loginName,  "-s", "manila", "-w"] []
    --case exitCode of
    --    ExitSuccess -> T.strip . T.pack $ "retrieve: " ++ output
    --    ExitFailure _ -> storePassword
    return ()
