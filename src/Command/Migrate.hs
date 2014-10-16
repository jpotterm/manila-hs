module Command.Migrate (migrateCommand, migrate, currentMigration, futureMigrations) where

import Control.Monad (when)
import Data.Char
import Database.HDBC
import Database.HDBC.Sqlite3
import System.IO
import System.Directory
import System.FilePath ((</>), splitExtension, takeFileName)

import Paths_manila
import Util


migrateCommand :: [String] -> String -> IO ()
migrateCommand args flags = do
    conn <- getDbConnection
    migrate conn
    disconnect conn

migrateFrom :: Connection -> Integer -> IO ()
migrateFrom conn from = do
    futureMigrationPaths <- futureMigrations from

    when (length futureMigrationPaths /= 0) $ do
        mapM_ (executeSqlFile conn) futureMigrationPaths
        let lastMigration = read (filter isDigit (takeFileName (last futureMigrationPaths))) :: Integer

        updateOrInsert conn
            ("SELECT * FROM schema_migration WHERE id = 1", [])
            ("UPDATE schema_migration SET migration_number=? WHERE id = 1", [])
            ("INSERT INTO schema_migration (migration_number) VALUES (?)", [toSql lastMigration])

        return ()

currentMigration :: Connection -> IO Integer
currentMigration conn = do
    currentMigrationResult <- trySql $ quickQuery' conn "SELECT * FROM schema_migration WHERE id = 1" []
    return $ case currentMigrationResult of
                  Left e   -> 0
                  Right [] -> 0
                  Right x  -> fromSql $ head (head x)

futureMigrations :: Integer -> IO [FilePath]
futureMigrations from = do
    dataDir <- getDataDir
    let migrationDir = dataDir </> "migrations"
    directoryFiles <- getDirectoryContents migrationDir
    let migrationFiles = filter (hasExtension ".sql") directoryFiles
    let futureMigrations = dropWhile (`stringLEInteger` from) migrationFiles
    return $ map (migrationDir </>) futureMigrations

hasExtension :: String -> FilePath -> Bool
hasExtension extension path =
    let (_, ext) = splitExtension path
    in  extension == ext

stringLEInteger :: String -> Integer -> Bool
s `stringLEInteger` i = read (filter isDigit s) <= i

migrate :: Connection -> IO ()
migrate conn = do
    from <- currentMigration conn
    migrateFrom conn from
