module Command.Summary.Calculate (calculateSummary) where

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


data TimeRule = TimeRule String Integer String UTCTime


calculateSummary :: IO (Integer, [Envelope])
calculateSummary = do
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

    timeRulesResult <- quickQuery'
        conn
        ("SELECT envelope.name, time_rule.amount, time_rule.frequency, time_rule.start"
            ++ " FROM envelope"
            ++ " LEFT OUTER JOIN time_rule ON time_rule.envelope_id = envelope.id"
            ++ " ORDER BY envelope.name ASC")
        []

    disconnect conn

    let timeRules = map timeRuleFromSql timeRulesResult

    let eBalancesMaybe = map calcEnvelopeBalance envelopesResult
    let eBalances = catMaybes eBalancesMaybe
    let eBalancesWithRules = map (calcRules timeRules) eBalances
    let totalRemainingBalance = totalAccountBalance - envelopeSum eBalancesWithRules

    return (totalAccountBalance, eBalancesWithRules)



timeRuleFromSql :: [SqlValue] -> TimeRule
timeRuleFromSql (eName:amount:frequency:start:[]) =
    TimeRule (fromSql eName) (fromSql amount) (fromSql frequency) (fromSql start)


calcRules :: [TimeRule] -> Envelope -> Envelope
calcRules rules envelope = calcRelevantRules (filter (equalEnvelope envelope) rules) envelope
    where equalEnvelope (Envelope eName _) (TimeRule rName _ _ _) = eName == rName


calcRelevantRules :: [TimeRule] -> Envelope -> Envelope
calcRelevantRules rules envelope = foldr calcRelevantRule envelope rules


calcRelevantRule :: TimeRule -> Envelope -> Envelope
calcRelevantRule (TimeRule _ rAmount frequency start) (Envelope eName eAmount) = Envelope eName (eAmount + rAmount)


envelopeSum :: [Envelope] -> Integer
envelopeSum = sum . map (\(Envelope name amount) -> amount)


calcEnvelopeBalance :: [SqlValue] -> Maybe Envelope
calcEnvelopeBalance (eName:eAmount:tAmount:[]) =
    case (safeFromSql eName, safeFromSql eAmount, safeFromSql tAmount) of
        (Right eNameConverted, Right eAmountConverted, Right tAmountConverted) ->
            Just (Envelope eNameConverted (eAmountConverted - tAmountConverted))
        (Right eNameConverted, Right eAmountConverted, _) ->
            Just (Envelope eNameConverted eAmountConverted)
        otherwise -> Nothing
