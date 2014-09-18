module Main where

import Data.List
import Data.Maybe
import System.Environment
import Text.PrettyPrint.Boxes

import Command.Attach
import Command.Categories
import Command.Envelope
import Command.Init
import Command.Migrate
import Command.Mint
import Command.Rule
import Command.Summary
import Command.Transfer
import Utils


data Command = Command { name :: String
                       , function :: [String] -> String -> IO ()
                       , docArgs :: String
                       , docDescription :: String
                       }

commands :: [Command]
commands = [ Command { name="init"
                     , function=initCommand
                     , docArgs=""
                     , docDescription="Initialize manila project in the current directory"
                     }
           , Command { name="migrate"
                     , function=migrateCommand
                     , docArgs=""
                     , docDescription="Migrate the database to a newer version of manila"
                     }
           , Command { name="help"
                     , function=helpCommand
                     , docArgs=""
                     , docDescription="Print this list"
                     }
           , Command { name="mint"
                     , function=mintCommand
                     , docArgs=""
                     , docDescription="Setup Mint.com account credentials"
                     }
           , Command { name="pull"
                     , function=pullCommand
                     , docArgs=""
                     , docDescription="Download and import transactions and account balances from Mint.com"
                     }
           , Command { name="envelope"
                     , function=envelopeCommand
                     , docArgs=""
                     , docDescription="Add an envelope"
                     }
           , Command { name="transfer"
                     , function=transferCommand
                     , docArgs=""
                     , docDescription="Transfer money to an envelope (use a negative amount to transfer from envelope)"
                     }
           , Command { name="summary"
                     , function=summaryCommand
                     , docArgs=""
                     , docDescription="Summary of envelope and account balances"
                     }
           , Command { name="attach"
                     , function=attachCommand
                     , docArgs=""
                     , docDescription="Attach Mint.com category to envelope"
                     }
           , Command { name="categories"
                     , function=categoriesCommand
                     , docArgs=""
                     , docDescription="List the categories that are attached to an envelope"
                     }
           , Command { name="rule"
                     , function=addRuleCommand
                     , docArgs=""
                     , docDescription="Add a rule"
                     }
           , Command { name="rules"
                     , function=listRulesCommand
                     , docArgs=""
                     , docDescription="List all rules"
                     }
           ]


helpCommand :: [String] -> String -> IO ()
helpCommand args flags = do
    let rows = map commandToRow commands
    let keyWidth = maximum . map (length . fst) $ rows
    let boxes = map (rowToBox keyWidth) rows
    printBox $ vcat left boxes


commandToRow :: Command -> (String, String)
commandToRow command = (name command, docDescription command)


rowToBox :: Int -> (String, String) -> Box
rowToBox keyWidth (key, help) = emptyBox 1 1 <> alignHoriz left keyWidth (text key) <> emptyBox 1 3 <> para left (76 - keyWidth) help


isFlag :: String -> Bool
isFlag (x:xs) = x == '-'


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
