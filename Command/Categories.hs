module Command.Categories (categoriesCommand) where

import Data.List
import Database.HDBC
import Database.HDBC.Sqlite3

import Utils


categoriesCommand :: [String] -> String -> IO ()
categoriesCommand (envelope:[]) flags = do
    conn <- getDbConnection

    envelopeId <- getEnvelopeId conn envelope
    categoriesResult <- quickQuery'
        conn
        ("SELECT category.name FROM category"
            ++ " INNER JOIN envelope_category ON category.id = envelope_category.category_id"
            ++ " WHERE envelope_category.envelope_id = ?")
        [toSql envelopeId]

    mapM_ (putStrLn . fromSql . head) categoriesResult

    disconnect conn
categoriesCommand _ _ = putStrLn "Command takes 1 argument"
