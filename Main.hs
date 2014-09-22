module Main where

import Data.Char (isLetter)
import Data.List
import Data.Maybe
import System.Environment

import Util
import CommandList
import CommandType


isFlag :: String -> Bool
isFlag (x:y:[]) = x == '-' && isLetter y
isFlag _ = False


dispatch :: [String] -> IO ()
dispatch (command:args) =
    let
        (rawFlags, otherArgs) = partition isFlag args
        flags = filter (/= '-') $ concat rawFlags
    in
        case find (\x -> name x == command) commands of
            Just action -> function action otherArgs flags
            Nothing -> putStrLn $ "\"" ++ command ++ "\" is not a valid command. See man page for proper usage."
dispatch [] = putStrLn "Must specify a command. See man page for proper usage."

main = getArgs >>= dispatch
