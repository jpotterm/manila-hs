module Command.Migrate (migrateCommand, migrate, currentMigration, futureMigrations) where

import Control.Monad (when)
import Data.Char
import Data.Maybe (fromJust)
import Data.UUID (toString)
import Data.UUID.V1 (nextUUID)
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
    migrate conn True
    disconnect conn


migrateFrom :: Connection -> Integer -> Bool -> IO ()
migrateFrom conn from createBackup = do
    futureMigrationPaths <- futureMigrations from

    when (length futureMigrationPaths /= 0) $ do
        when createBackup backupProject
        runMigrations conn futureMigrationPaths


runMigrations :: Connection -> [FilePath] -> IO ()
runMigrations conn migrationPaths = do
    mapM_ (executeSqlFile conn) migrationPaths
    let lastMigration = read (filter isDigit (takeFileName (last migrationPaths))) :: Integer

    updateOrInsert conn
        ("SELECT * FROM schema_migration WHERE id = 1", [])
        ("UPDATE schema_migration SET migration_number=? WHERE id = 1", [])
        ("INSERT INTO schema_migration (migration_number) VALUES (?)", [toSql lastMigration])

    return ()


backupProject :: IO ()
backupProject = do
    uuid <- nextUUID
    let backupFileName = "manila_backup_" ++ (toString $ fromJust uuid) ++ ".db"
    copyFile "manila.db" backupFileName
    putStrLn $ "A backup of this project has been saved to " ++ backupFileName


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


migrate :: Connection -> Bool -> IO ()
migrate conn createBackup = do
    from <- currentMigration conn
    migrateFrom conn from createBackup
