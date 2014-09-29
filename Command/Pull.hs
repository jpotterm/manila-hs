{-# LANGUAGE OverloadedStrings #-}

module Command.Pull (pullCommand) where

import System.Directory
import System.Posix.User
import System.Process

import Mint.Account
import Mint.Category
import Mint.Login
import Mint.Transaction
import qualified Settings
import Util


pullCommand :: [String] -> String -> IO ()
pullCommand args flags = do
    username <- prompt "Mint.com username: "
    password <- promptPassword "Mint.com password: "

    (session, tokenSession) <- mintLogin username password
    mintAccounts tokenSession
    mintCategories session
    mintTransactions session
