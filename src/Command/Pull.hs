{-# LANGUAGE OverloadedStrings #-}

module Command.Pull (pullCommand) where

import Data.Foldable (mapM_)
import qualified Network.Wreq as Wreq
import Prelude hiding (mapM_)
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

    loginResult <- mintLogin username password
    mapM_ doImport loginResult

doImport :: (Wreq.Options, Wreq.Options) -> IO ()
doImport (session, tokenSession) = do
    mintAccounts tokenSession
    mintCategories session
    mintTransactions session
