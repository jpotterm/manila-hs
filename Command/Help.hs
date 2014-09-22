module Command.Help where

import Data.List
import Data.Maybe
import Text.PrettyPrint.Boxes

import {-# SOURCE #-} CommandList
import CommandType


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
