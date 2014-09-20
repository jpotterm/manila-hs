{-# LANGUAGE FlexibleContexts #-}

module Util
    ( executeSqlFile
    , updateOrInsert
    , trySql
    , prompt
    , getOrCreate
    , getDbConnection
    , getCategoryId
    , getAccountId
    , getInstanceUUID
    , getEnvelopeId
    , defaultFromSql
    , readInteger100
    , promptPassword
    , parseLocalTime
    ) where

import Control.Exception
import Control.Monad
import Data.Char
import Data.Convertible
import Data.Maybe
import Data.Time
import Data.UUID (UUID, fromString)
import Database.HDBC
import Database.HDBC.Sqlite3
import System.IO
import System.Locale (defaultTimeLocale)


getDbConnection :: IO Connection
getDbConnection = do
    conn <- connectSqlite3 "project/manila.db"
    runRaw conn "COMMIT; PRAGMA foreign_keys = ON; BEGIN TRANSACTION"
    return conn


executeSqlFile :: Connection -> FilePath -> IO ()
executeSqlFile conn file = do
    contents <- readFile file
    runRaw conn contents
    commit conn


updateOrInsert :: Connection -> (String, [SqlValue]) -> (String, [SqlValue]) -> (String, [SqlValue]) -> IO Integer
updateOrInsert conn (selectSql, selectParams) (updateSql, updateParams) (insertSql, insertParams) = do
	selectResult <- quickQuery' conn selectSql selectParams
	let result = case selectResult of
		[] -> run conn insertSql insertParams
		_ -> run conn updateSql updateParams
	commit conn
	result


getOrCreate :: Connection -> (String, [SqlValue]) -> (String, [SqlValue]) -> IO [[SqlValue]]
getOrCreate conn (selectSql, selectParams) (insertSql, insertParams) = do
    selectResultMaybe <- quickQuery' conn selectSql selectParams

    when (null selectResultMaybe) $ do
        run conn insertSql insertParams
        commit conn

    --TODO return selectResultMaybe instead
    quickQuery' conn selectSql selectParams


trySql :: IO a -> IO (Either SqlError a)
trySql = try


prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine


getCategoryId :: Connection -> String -> IO Integer
getCategoryId conn category = do
    selectResult <- quickQuery' conn "SELECT id FROM category WHERE name = ?" [toSql category]
    return . fromSql . head . head $ selectResult


getAccountId :: Connection -> String -> IO Integer
getAccountId conn account = do
    selectResult <- quickQuery' conn "SELECT id FROM account WHERE name = ?" [toSql account]
    return . fromSql . head . head $ selectResult


getInstanceUUID :: Connection -> IO UUID
getInstanceUUID conn = do
    result <- quickQuery' conn "SELECT instance_uuid FROM instance_uuid WHERE id = 1" []
    return . fromJust . fromString . fromSql . head . head $ result


getEnvelopeId :: Connection -> String -> IO Integer
getEnvelopeId conn name = do
    envelopeResult <- quickQuery' conn "SELECT id FROM envelope WHERE name = ?" [toSql name]
    return $ fromSql . head . head $ envelopeResult


defaultFromSql :: Convertible SqlValue a => SqlValue -> a -> a
defaultFromSql x defaultValue =
    case safeFromSql x of
        Left (ConvertError{}) -> defaultValue
        Right a -> a


readInteger100 :: String -> Integer
readInteger100 s = truncate $ (read s :: Float) * 100


promptPassword :: String -> IO String
promptPassword message = do
    putStr message
    hFlush stdout
    pass <- withEcho False getLine
    putChar '\n'
    return pass

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
    old <- hGetEcho stdin
    bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action


parseLocalTime :: TimeZone -> String -> Maybe UTCTime
parseLocalTime tz s = fmap (localTimeToUTC tz) $ parseTime defaultTimeLocale "%F %T" s
