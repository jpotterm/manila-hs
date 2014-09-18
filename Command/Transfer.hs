module Command.Transfer (transferCommand) where

import Database.HDBC
import Database.HDBC.Sqlite3

import Util


transferCommand :: [String] -> String -> IO ()
transferCommand (amountString:envelope:[]) flags = do
    conn <- getDbConnection

    let positiveAmount = readInteger100 amountString

    let amount = if 'r' `elem` flags then -1 * positiveAmount else positiveAmount

    currentAmountResult <- quickQuery' conn "SELECT amount FROM envelope WHERE name = ?" [toSql envelope]
    let currentAmount = fromSql . head . head $ currentAmountResult
    let totalAmount = currentAmount + amount

    run conn "UPDATE envelope SET amount = ? WHERE name = ?" [toSql totalAmount, toSql envelope]

    commit conn
    disconnect conn
transferCommand _ _ = putStrLn "Command takes 2 arguments"
