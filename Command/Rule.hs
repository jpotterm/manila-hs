{-# LANGUAGE OverloadedStrings #-}

module Command.Rule (listRulesCommand, addRuleCommand) where

import Control.Monad
import Data.Maybe
import Data.List
import Data.Time
import Data.Time.Recurrence
import Database.HDBC

import Settings
import Util


listRulesCommand :: [String] -> String -> IO ()
listRulesCommand args flags = do
    conn <- getDbConnection

    categoryResult <- quickQuery'
        conn
        ("SELECT envelope.name, category.name, category_rule.amount FROM category_rule"
            ++ " INNER JOIN envelope ON category_rule.envelope_id = envelope.id"
            ++ " INNER JOIN category ON category_rule.category_id = category.id"
            ++ " ORDER BY envelope.name, category.name ASC")
        []

    timeResult <- quickQuery'
        conn
        ("SELECT envelope.name, time_rule.schedule, time_rule.amount FROM time_rule"
            ++ " INNER JOIN envelope ON time_rule.envelope_id = envelope.id"
            ++ " ORDER BY envelope.name ASC")
        []

    disconnect conn

    mapM_ (putStrLn . showCategoryRule) categoryResult

    putStrLn ""

    mapM_ (putStrLn . showTimeRule) timeResult


showCategoryRule :: [SqlValue] -> String
showCategoryRule (eName:cName:amount:[]) =
    "Envelope: " ++ fromSql eName ++ " | category: " ++ fromSql cName ++ " | amount: " ++ fromSql amount


showTimeRule :: [SqlValue] -> String
showTimeRule (eName:schedule:amount:[]) =
    "Envelope: " ++ fromSql eName ++ " | schedule: " ++ fromSql schedule ++ " | amount: " ++ fromSql amount


addRuleCommand :: [String] -> String -> IO ()
addRuleCommand args flags
    | 'c' `elem` flags = categoryRule args
    | 't' `elem` flags = timeRule args
    | otherwise = putStrLn "Command requires one of the following flags: -c, -t"


categoryRule :: [String] -> IO ()
categoryRule (envelope:category:amount:[]) = do
    conn <- getDbConnection

    envelopeId <- getEnvelopeId conn envelope
    categoryId <- getCategoryId conn category

    run conn
        "INSERT INTO category_rule (envelope_id, category_id, amount) VALUES (?, ?, ?)"
        [toSql envelopeId, toSql categoryId, toSql amount]

    commit conn
    disconnect conn

categoryRule _ = putStrLn "Command requires 3 arguments"


showKeys :: [(String, a)] -> String
showKeys = concat . (intersperse ", ") . fst . unzip


timeRule :: [String] -> IO ()
timeRule (envelope:frequency:amountString:[]) = do
    now <- getCurrentTime
    addTimeRule envelope frequency amountString now

timeRule (envelope:frequency:amountString:startString:[]) = do
    tz <- getCurrentTimeZone
    now <- getCurrentTime
    let start = fromMaybe now $ parseLocalTime tz startString

    addTimeRule envelope frequency amountString start

timeRule _ = putStrLn "Command requires at least 3 arguments"


addTimeRule :: String -> String -> String -> UTCTime -> IO ()
addTimeRule envelope frequency amountString start = do
    conn <- getDbConnection

    envelopeId <- getEnvelopeId conn envelope
    let amount = readInteger100 amountString

    case lookup frequency frequencyMap of
        Just _ -> void $ run conn
                             "INSERT INTO time_rule (envelope_id, frequency, amount, start) VALUES (?, ?, ?, ?)"
                             [toSql envelopeId, toSql frequency, toSql amount, toSql start]
        Nothing -> putStrLn $ "Incorrect frequency. Possible options are: " ++ showKeys frequencyMap

    commit conn
    disconnect conn
