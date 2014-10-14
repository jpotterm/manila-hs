module Command.Envelope (envelopeCommand) where

import Database.HDBC
import Database.HDBC.Sqlite3

import Util


envelopeCommand :: [String] -> String -> IO ()
envelopeCommand (name:[]) flags = do
    conn <- getDbConnection

    if 'r' `elem` flags
        then deleteEnvelope conn name
        else createEnvelope conn name

    commit conn
    disconnect conn
envelopeCommand _ _ = putStrLn "Command requires 2 arguments"


createEnvelope :: Connection -> String -> IO Integer
createEnvelope conn name =
    run conn "INSERT INTO envelope (name) VALUES (?)" [toSql name]


deleteEnvelope :: Connection -> String -> IO Integer
deleteEnvelope conn name =
    run conn "DELETE FROM envelope WHERE name = ?" [toSql name]
