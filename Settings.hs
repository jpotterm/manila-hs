module Settings (frequencyMap) where

import Data.Time.Recurrence


frequencyMap :: [(String, Freq)]
frequencyMap = [ ("daily", daily)
               , ("weekly", weekly)
               , ("monthly", monthly)
               , ("yearly", yearly)
               ]
