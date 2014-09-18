{-# LANGUAGE OverloadedStrings #-}

module Command.Rule (listRulesCommand, addRuleCommand) where

import Database.HDBC

import Utils


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

    --conn <- getDbConnection
    --instanceUUID <- getInstanceUUID conn

    --username <- prompt "Mint.com username: "
    --password <- prompt "Mint.com password: "

    --let arguments = [ "add-generic-password"
    --                , "-a", username
    --                , "-s", "manila:" ++ toString instanceUUID
    --                , "-w", password
    --                , "-U"
    --                ]

    --(exitCode, output, _) <- readProcessWithExitCode "security" arguments []
    --disconnect conn


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


timeRule :: [String] -> IO ()
timeRule (envelope:schedule:amount:[]) = do
    conn <- getDbConnection

    envelopeId <- getEnvelopeId conn envelope

    run conn
        "INSERT INTO time_rule (envelope_id, schedule, amount) VALUES (?, ?, ?)"
        [toSql envelopeId, toSql schedule, toSql amount]

    commit conn
    disconnect conn

timeRule _ = putStrLn "Command requires 3 arguments"

