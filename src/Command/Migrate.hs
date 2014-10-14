module Command.Migrate (migrateCommand, migrate) where

import Data.Char
import Database.HDBC
import Database.HDBC.Sqlite3
import System.IO
import System.Directory
import System.FilePath ((</>), splitExtension)

import Paths_manila
import Util


migrateCommand :: [String] -> String -> IO ()
migrateCommand args flags = do
    conn <- getDbConnection
    migrate conn
    disconnect conn

migrateFrom :: Connection -> Integer -> IO ()
migrateFrom conn from = do
    dataDir <- getDataDir
    let migrationDir = dataDir </> "migrations"
    directoryFiles <- getDirectoryContents migrationDir
    let migrationFiles = filter (hasExtension ".sql") directoryFiles
    let futureMigrations = dropWhile (`stringLTInteger` from) migrationFiles
    let futureMigrationPaths = map (migrationDir </>) futureMigrations
    mapM_ (executeSqlFile conn) futureMigrationPaths
    let lastMigration = read (filter isDigit (last futureMigrations)) :: Integer

    updateOrInsert conn
        ("SELECT * FROM schema_migration WHERE id = 1", [])
        ("UPDATE schema_migration SET migration_number=? WHERE id = 1", [])
        ("INSERT INTO schema_migration (migration_number) VALUES (?)", [toSql lastMigration])

    return ()

hasExtension :: String -> FilePath -> Bool
hasExtension extension path =
    let (_, ext) = splitExtension path
    in  extension == ext

stringLTInteger :: String -> Integer -> Bool
s `stringLTInteger` i = read (filter isDigit s) < i

migrate :: Connection -> IO ()
migrate conn = do
    currentMigrationResult <- trySql $ quickQuery' conn "SELECT * FROM schema_migration WHERE id = 1" []
    let currentMigration = case currentMigrationResult of
                                Left e -> 0
                                Right [] -> 0
                                Right x -> fromSql $ head (head x)
    migrateFrom conn currentMigration
