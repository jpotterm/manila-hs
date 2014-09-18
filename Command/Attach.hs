module Command.Attach (attachCommand) where

import Database.HDBC
import Database.HDBC.Sqlite3

import Utils


attachCommand :: [String] -> String -> IO ()
attachCommand (envelope:category:[]) flags = do
    conn <- getDbConnection

    envelopeId <- getEnvelopeId conn envelope
    categoryId <- getCategoryId conn category

    if 'r' `elem` flags
        then detach conn envelopeId categoryId
        else attach conn envelopeId categoryId

    commit conn
    disconnect conn
attachCommand _ _ = putStrLn "Command takes 2 arguments"


attach :: Connection -> Integer -> Integer -> IO Integer
attach conn envelopeId categoryId =
    run conn "INSERT INTO envelope_category (envelope_id, category_id) VALUES (?, ?)" [toSql envelopeId, toSql categoryId]


detach :: Connection -> Integer -> Integer -> IO Integer
detach conn envelopeId categoryId =
    run conn "DELETE FROM envelope_category WHERE envelope_id = ? AND category_id = ?" [toSql envelopeId, toSql categoryId]
