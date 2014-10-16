module Main where

import Control.Monad (when)
import Data.Char (isLetter)
import Data.List
import Data.Maybe
import System.Directory (doesFileExist)
import System.Environment

import Util
import CommandList
import CommandType
import Command.Migrate (currentMigration, futureMigrations)


nonDatabaseCommands :: [String]
nonDatabaseCommands = ["init", "help"]


validate :: String -> IO Bool
validate command = do
    dbExists <- doesFileExist "manila.db"
    validateDatabaseExists command dbExists
    validateMigrations dbExists
    return $ validateContinueCommand command dbExists


validateContinueCommand :: String -> Bool -> Bool
validateContinueCommand command dbExists = dbExists || command `elem` nonDatabaseCommands


validateDatabaseExists :: String -> Bool -> IO ()
validateDatabaseExists command dbExists =
    let needsDatabase = not $ command `elem` nonDatabaseCommands
    in  when (not dbExists && needsDatabase) $
             putStrLn "Could not find 'manila.db' in this directory. Run 'manila init' to create a new manila project in this directory."


validateMigrations :: Bool -> IO ()
validateMigrations dbExists =
    when dbExists $ do
        conn <- getDbConnection
        from <- currentMigration conn
        migrations <- futureMigrations from
        when (length migrations /= 0) $ putStrLn "You should migrate"


isFlag :: String -> Bool
isFlag (x:y:[]) = x == '-' && isLetter y
isFlag _ = False


dispatch :: [String] -> IO ()
dispatch (command:args) = do
    let (rawFlags, otherArgs) = partition isFlag args
    let flags = filter (/= '-') $ concat rawFlags
    valid <- validate command
    when valid $ case find (\x -> name x == command) commands of
                      Just action -> function action otherArgs flags
                      Nothing     -> putStrLn $ "\"" ++ command ++ "\" is not a valid command. Run 'manila help' for proper usage."
dispatch [] = putStrLn "You must specify a command. Run 'manila help' for proper usage."


main = getArgs >>= dispatch
