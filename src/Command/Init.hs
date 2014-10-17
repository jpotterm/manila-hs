module Command.Init (initCommand) where

import Data.Maybe
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Directory (doesFileExist)

import Util
import Command.Migrate


initCommand :: [String] -> String -> IO ()
initCommand args flags = do
    dbExists <- doesFileExist "manila.db"

    if dbExists
        then putStrLn "There is already a manila project in this directory."
        else do
            conn <- getDbConnection
            migrate conn False
            commit conn
            disconnect conn
