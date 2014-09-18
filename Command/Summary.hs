module Command.Summary (summaryCommand) where

import Data.Either
import Data.List
import Data.Maybe
import Database.HDBC
import Database.HDBC.Sqlite3
import Text.PrettyPrint.Boxes

import Currency
import Util


data SummaryRow a = Header a | Divider | Body a | Blank


summaryCommand :: [String] -> String -> IO ()
summaryCommand args flags = do
    conn <- getDbConnection

    accountsResult <- quickQuery' conn "SELECT SUM(account.balance) FROM account" []
    let totalAccountBalance = fromSql . head . head $ accountsResult

    envelopesResult <- quickQuery'
        conn
        ("SELECT envelope.name, envelope.amount, SUM([transaction].amount)"
            ++ " FROM envelope"
            ++ " LEFT OUTER JOIN envelope_category ON envelope_category.envelope_id = envelope.id"
            ++ " LEFT OUTER JOIN category ON category.id = envelope_category.category_id OR category.parent_id = envelope_category.category_id"
            ++ " LEFT OUTER JOIN [transaction] ON [transaction].category_id = category.id"
            ++ " GROUP BY envelope.name, envelope.amount"
            ++ " ORDER BY envelope.name ASC")
        []

    disconnect conn

    let eBalancesMaybe = map calcEnvelopeBalance envelopesResult
    let eBalances = map removeMaybes $ filter (not . hasNothingTuple) eBalancesMaybe
    let summarizedEnvelopes = map summarizeEnvelope eBalances

    let totalRemainingBalance = totalAccountBalance - envelopeSum eBalances
    let totalRow = ("Remaining Balance", formatCurrency totalRemainingBalance)

    let summary = (Header totalRow:Divider:summarizedEnvelopes)

    let leftWidth = maximum $ map summaryRowLeftWidth summary
    let rightWidth = maximum $ map summaryRowRightWidth summary

    printBox $ vcat left $ map (boxRow leftWidth rightWidth) summary


envelopeSum :: [(String, Integer)] -> Integer
envelopeSum = sum . map (\(name, amount) -> amount)


calcEnvelopeBalance :: [SqlValue] -> (Maybe String, Maybe Integer)
calcEnvelopeBalance (eName:eAmount:tAmount:[]) =
    case (safeFromSql eName, safeFromSql eAmount, safeFromSql tAmount) of
        (Right eNameConverted, Right eAmountConverted, Right tAmountConverted) ->
            (Just eNameConverted, Just $ eAmountConverted - tAmountConverted)
        (Right eNameConverted, Right eAmountConverted, _) ->
            (Just eNameConverted, Just eAmountConverted)
        (_, _, _) -> (Nothing, Nothing)


summarizeEnvelope :: (String, Integer) -> SummaryRow (String, String)
summarizeEnvelope (name, balance) = Body (name, formatCurrency balance)


hasNothingTuple :: (Maybe String, Maybe Integer) -> Bool
hasNothingTuple (a, b) = isNothing a && isNothing b


removeMaybes :: (Maybe String, Maybe Integer) -> (String, Integer)
removeMaybes (a, b) = (fromJust a, fromJust b)


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
