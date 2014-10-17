module Command.Init (initCommand) where

import Data.Maybe
import Database.HDBC
import Database.HDBC.Sqlite3

import Util
import Command.Migrate


initCommand :: [String] -> String -> IO ()
initCommand args flags = do
    conn <- getDbConnection
    migrate conn False
    commit conn
    disconnect conn
