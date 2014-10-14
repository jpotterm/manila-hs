module CommandType (Command(Command, name, function, docSummary, doc)) where

data Command = Command { name :: String
                       , function :: [String] -> String -> IO ()
                       , docSummary :: String
                       , doc :: String
                       }
