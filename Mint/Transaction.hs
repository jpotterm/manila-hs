{-# LANGUAGE OverloadedStrings #-}

module Mint.Transaction (importTransactions, deleteTransactions) where

import Data.Time.Clock
import Data.Time.Format
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Locale
import Text.CSV

import Utils


importTransactions :: IO ()
importTransactions = do
    parseResult <- parseCSVFromFile "project/transactions.csv"
    case parseResult of
        Left _ -> putStrLn "Could not parse transactions CSV file."
        Right csv -> importCSV (tail csv)


deleteTransactions :: IO ()
deleteTransactions = do
    conn <- getDbConnection

    run conn "DELETE FROM [transaction]" []
    commit conn


importCSV :: CSV -> IO ()
importCSV csv = do
    conn <- getDbConnection

    mapM_ (importRecord conn) csv

    commit conn
    disconnect conn


importRecord :: Connection -> Record -> IO ()
importRecord conn (date:description:originalDescription:amount:_:category:account:_:_) = do
    categoryId <- getCategoryId conn category

    let amountInteger = readInteger100 amount
    let dateMaybe = parseTime defaultTimeLocale "%-m/%-d/%Y" date :: Maybe UTCTime
    let dateString = maybe "" (formatTime defaultTimeLocale "%Y-%m-%d") dateMaybe

    run conn
        "INSERT INTO [transaction] ([date], description, original_description, amount, category_id) VALUES (?, ?, ?, ?, ?)"
        [toSql dateString, toSql description, toSql originalDescription, toSql amountInteger, toSql categoryId]

    return ()
importRecord conn _ = return ()


