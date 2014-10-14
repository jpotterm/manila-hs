module Settings (frequencyMap, mintHostname) where

import Data.Time.Recurrence


frequencyMap :: [(String, Freq)]
frequencyMap = [ ("daily", daily)
               , ("weekly", weekly)
               , ("monthly", monthly)
               , ("yearly", yearly)
               ]

mintHostname :: String
mintHostname = "https://wwws.mint.com"
