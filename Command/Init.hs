module Command.Init (initCommand) where

import Data.Maybe
import Data.UUID
import Data.UUID.V1
import Database.HDBC
import Database.HDBC.Sqlite3

import Util
import Command.Migrate


initCommand :: [String] -> String -> IO ()
initCommand args flags = do
    conn <- getDbConnection
    uuid <- nextUUID

    migrate conn
    run conn "INSERT INTO instance_uuid (id, instance_uuid) VALUES (1, ?)" [toSql . toString . fromJust $ uuid]
    commit conn
    disconnect conn
