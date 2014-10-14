module Command.Summary (summaryCommand) where

import Text.PrettyPrint.Boxes

import Command.Summary.Calculate (calculateSummary)
import Command.Summary.Format (formatSummary)


summaryCommand :: [String] -> String -> IO ()
summaryCommand args flags = do
    (remainingBalance, envelopes) <- calculateSummary
    printBox $ formatSummary remainingBalance envelopes
