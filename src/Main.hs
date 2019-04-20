module Main where

import Control.Conditional ((<&&>))
import Control.Monad (when)
import Data.Char (isLetter)
import Data.List
import Data.Maybe
import System.Directory (doesFileExist)
import System.Environment

import Util
import CommandList
import CommandType
import Command.Migrate (currentMigration, migrationRequired)


validate :: String -> IO Bool
validate command = validateDatabaseExists command <&&> validateMigrations command


validateDatabaseExists :: String -> IO Bool
validateDatabaseExists command
    | command `elem` ["init", "help"] = return True
    | otherwise = do
        dbExists <- doesFileExist "manila.db"
        if dbExists
            then return True
            else do
                putStrLn "Could not find 'manila.db' in this directory. Run 'manila init' to create a new manila project in this directory."
                return False


validateMigrations :: String -> IO Bool
validateMigrations command
    | command `elem` ["init", "help", "migrate"] = return True
    | otherwise = do
        conn <- getDbConnection
        required <- migrationRequired conn
        when (not required) $ putStrLn "This project was created by an older version of manila. Run 'manila migrate' to upgrade it."
        return required


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
