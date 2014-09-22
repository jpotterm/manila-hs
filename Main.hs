module Main where

import Data.Char (isLetter)
import Data.List
import Data.Maybe
import System.Environment
import Text.PrettyPrint.Boxes

import Command.Attach
import Command.Categories
import Command.Envelope
import Command.Init
import Command.Migrate
import Command.Pull
import Command.Rule
import Command.Summary
import Command.Transfer
import Util


data Command = Command { name :: String
                       , function :: [String] -> String -> IO ()
                       , docSummary :: String
                       , doc :: String
                       }

commands :: [Command]
commands = [ Command { name="init"
                     , function=initCommand
                     , docSummary="Initialize manila project in the current directory"
                     , doc="manila init\n\nInitialize manila project in the current directory."
                     }
           , Command { name="migrate"
                     , function=migrateCommand
                     , docSummary="Migrate the database to a newer version of manila"
                     , doc="manila migrate\n\nMigrate the database to a newer version of manila."
                     }
           , Command { name="help"
                     , function=helpCommand
                     , docSummary="Print this list"
                     , doc="manila help\n\nPrint this list."
                     }
           , Command { name="pull"
                     , function=pullCommand
                     , docSummary="Download and import transactions and account balances from Mint.com"
                     , doc="manila pull\n\nDownload and import transactions and account balances from Mint.com."
                     }
           , Command { name="envelope"
                     , function=envelopeCommand
                     , docSummary="Add an envelope"
                     , doc="manila envelope <name>\n\nAdd an envelope."
                     }
           , Command { name="transfer"
                     , function=transferCommand
                     , docSummary="Transfer money to an envelope (use a negative amount to transfer from envelope)"
                     , doc="manila transfer <amount> <envelope>\n\nTransfer money to an envelope (use a negative amount to transfer from envelope)."
                     }
           , Command { name="summary"
                     , function=summaryCommand
                     , docSummary="Summary of envelope and account balances"
                     , doc="manila summary\n\nSummary of envelope and account balances."
                     }
           , Command { name="attach"
                     , function=attachCommand
                     , docSummary="Attach Mint.com category to envelope"
                     , doc="manila attach <envelope> <category>\n\nAttach Mint.com category to envelope."
                     }
           , Command { name="categories"
                     , function=categoriesCommand
                     , docSummary="List the categories that are attached to an envelope"
                     , doc="manila categories\n\nList the categories that are attached to an envelope."
                     }
           , Command { name="rule"
                     , function=addRuleCommand
                     , docSummary="Add a rule"
                     , doc="manila rule -c <envelope> <category> <percentage> <amount> [start_date]\nmanila rule -t <envelope> <frequence> <amount> [start_date]\n\nAdd a rule."
                     }
           , Command { name="rules"
                     , function=listRulesCommand
                     , docSummary="List all rules"
                     , doc="manila rules\n\nList all rules."
                     }
           ]


helpCommand :: [String] -> String -> IO ()
helpCommand args flags = do
    putStrLn $ case args of
        [] -> helpSummary
        (command:[]) -> helpForCommand command
        otherwise -> "Command takes at most 1 argument."


helpSummary :: String
helpSummary =
    let rows = map commandToRow commands
        keyWidth = maximum . map (length . fst) $ rows
        boxes = map (rowToBox keyWidth) rows
    in  render $ vcat left boxes


helpForCommand :: String -> String
helpForCommand command =
    let commandObjectMaybe = find (\x -> name x == command) commands
        docMaybe = fmap doc commandObjectMaybe
    in  fromMaybe (command ++ " is not a command.") docMaybe


commandToRow :: Command -> (String, String)
commandToRow command = (name command, docSummary command)


rowToBox :: Int -> (String, String) -> Box
rowToBox keyWidth (key, help) = emptyBox 1 1 <> alignHoriz left keyWidth (text key) <> emptyBox 1 3 <> para left (76 - keyWidth) help


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
