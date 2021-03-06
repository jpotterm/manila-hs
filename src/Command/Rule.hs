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


-- TODO: Display error instead of defaulting to now if startString is unparseable
-- TODO: Allow startString to be either a date or datetime
categoryRule :: [String] -> IO ()
categoryRule (envelope:category:percentageString:amountString:rest) = do
    conn <- getDbConnection

    envelopeId <- getEnvelopeId conn envelope
    categoryId <- getCategoryId conn category

    let percentage = read percentageString :: Double
    let amount = readInteger100 amountString

    now <- getCurrentTime
    tz <- getCurrentTimeZone
    let start = case rest of
                     [] -> now
                     (startString:[]) -> fromMaybe now $ parseLocalTime tz startString

    run conn
        "INSERT INTO category_rule (envelope_id, category_id, percentage, amount, start) VALUES (?, ?, ?, ?, ?)"
        [toSql envelopeId, toSql categoryId, toSql percentage, toSql amount, toSql start]

    commit conn
    disconnect conn

categoryRule _ = putStrLn "Command requires at least 4 arguments"


showKeys :: [(String, a)] -> String
showKeys = concat . (intersperse ", ") . fst . unzip


timeRule :: [String] -> IO ()
timeRule (envelope:frequency:amountString:rest) = do
    conn <- getDbConnection

    envelopeId <- getEnvelopeId conn envelope
    let amount = readInteger100 amountString

    now <- getCurrentTime
    tz <- getCurrentTimeZone
    let start = case rest of
                     [] -> now
                     (startString:[]) -> fromMaybe now $ parseLocalTime tz startString

    case lookup frequency frequencyMap of
        Just _ -> void $ run conn
                             "INSERT INTO time_rule (envelope_id, frequency, amount, start) VALUES (?, ?, ?, ?)"
                             [toSql envelopeId, toSql frequency, toSql amount, toSql start]
        Nothing -> putStrLn $ "Incorrect frequency. Possible options are: " ++ showKeys frequencyMap

    commit conn
    disconnect conn
