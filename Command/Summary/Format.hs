module Command.Summary.Format (formatSummary) where

import Data.Either
import Data.List
import Data.Maybe
import Data.Time
import Database.HDBC
import Database.HDBC.Sqlite3
import Text.PrettyPrint.Boxes

import Command.Summary.Types
import Currency
import Util


data SummaryRow a = Header a | Divider | Body a | Blank


formatSummary :: Integer -> [Envelope] -> Box
formatSummary remainingBalance envelopes =
    let summarizedEnvelopes = map summarizeEnvelope envelopes
        totalRow = ("Remaining Balance", formatCurrency remainingBalance)
        summary = (Header totalRow:Divider:summarizedEnvelopes)
        leftWidth = maximum $ map summaryRowLeftWidth summary
        rightWidth = maximum $ map summaryRowRightWidth summary
    in  vcat left $ map (boxRow leftWidth rightWidth) summary


summarizeEnvelope :: Envelope -> SummaryRow (String, String)
summarizeEnvelope (Envelope _ name balance) = Body (name, formatCurrency balance)


summaryRowLeftWidth :: SummaryRow (String, String) -> Int
summaryRowLeftWidth (Header (left, _)) = length left
summaryRowLeftWidth (Body (left, _)) = length left
summaryRowLeftWidth _ = 0


summaryRowRightWidth :: SummaryRow (String, String) -> Int
summaryRowRightWidth (Header (_, right)) = length right
summaryRowRightWidth (Body (_, right)) = length right
summaryRowRightWidth _ = 0


boxRow :: Int -> Int -> SummaryRow (String, String) -> Box
boxRow leftWidth rightWidth (Header (leftValue, rightValue)) =
    alignHoriz right leftWidth (text leftValue)
        <> text "  :  "
        <> alignHoriz right rightWidth (text rightValue)

boxRow leftWidth rightWidth (Body (leftValue, rightValue)) =
    alignHoriz right leftWidth (text leftValue)
        <> text "  :  "
        <> alignHoriz right rightWidth (text rightValue)

boxRow leftWidth rightWidth Divider =
    text (replicate (leftWidth + 2) '-')
        <> text ":"
        <> text (replicate (rightWidth + 2) '-')

boxRow leftWidth rightWidth Blank = emptyBox 1 0
