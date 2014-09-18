{-# LANGUAGE OverloadedStrings #-}

module Command.Pull (pullCommand) where

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
import Util


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
