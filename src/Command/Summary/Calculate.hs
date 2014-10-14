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
import Settings
import Util

-- amount, frequency, start
data TimeRule = TimeRule Integer String UTCTime

-- percentage, rule amount, start, transaction amount
data CategoryRule = CategoryRule Double Integer UTCTime Integer


calculateSummary :: IO (Integer, [Envelope])
calculateSummary = do
    conn <- getDbConnection

    accountsResult <- quickQuery' conn "SELECT SUM(account.balance) FROM account" []
    let totalAccountBalance = fromSql . head . head $ accountsResult

    envelopesResult <- quickQuery'
        conn
        ("SELECT envelope.id, envelope.name, envelope.amount, SUM([transaction].amount)"
            ++ " FROM envelope"
            ++ " LEFT OUTER JOIN envelope_category ON envelope_category.envelope_id = envelope.id"
            ++ " LEFT OUTER JOIN category ON category.id = envelope_category.category_id OR category.parent_id = envelope_category.category_id"
            ++ " LEFT OUTER JOIN [transaction] ON [transaction].category_id = category.id"
            ++ " GROUP BY envelope.name, envelope.amount"
            ++ " ORDER BY envelope.name ASC")
        []

    let eBalances = mapMaybe calcEnvelopeBalance envelopesResult
    eBalancesWithRules <- mapM (calcRules' conn) eBalances

    disconnect conn

    let totalRemainingBalance = totalAccountBalance - envelopeSum eBalancesWithRules
    return (totalRemainingBalance, eBalancesWithRules)


calcRules' :: Connection -> Envelope -> IO Envelope
calcRules' conn envelope@(Envelope eId eName _) = do
    now <- getCurrentTime

    -- TODO: query based on start date?
    timeRulesResult <- quickQuery'
        conn
        ("SELECT time_rule.amount, time_rule.frequency, time_rule.start"
            ++ " FROM time_rule"
            ++ " WHERE time_rule.envelope_id = ?")
        [toSql eId]

    -- TODO: query based on start date
    categoryRulesResult <- quickQuery'
        conn
        ("SELECT category_rule.percentage, category_rule.amount, category_rule.start, [transaction].amount"
            ++ " FROM category_rule"
            ++ " INNER JOIN [transaction] ON category_rule.category_id = [transaction].category_id"
            ++ " WHERE category_rule.envelope_id = ? AND [transaction].[date] >= category_rule.start")
        [toSql eId]

    let timeRules = map timeRuleFromSql timeRulesResult
    let categoryRules = map categoryRuleFromSql categoryRulesResult

    let timeRuleEnvelope = foldr (calcTimeRule' now) envelope timeRules
    let categoryRuleEnvelope = foldr calcCategoryRule' timeRuleEnvelope categoryRules
    return categoryRuleEnvelope


-- TODO: don't use fromJust, instead convert from database immediately
calcTimeRule' :: UTCTime -> TimeRule -> Envelope -> Envelope
calcTimeRule' now (TimeRule rAmount frequency start) (Envelope eId eName eAmount) =
    let n = numberOfOccurrences start now (fromJust (lookup frequency frequencyMap))
    in  Envelope eId eName (eAmount + n * rAmount)


calcCategoryRule' :: CategoryRule -> Envelope -> Envelope
calcCategoryRule' (CategoryRule percentage rAmount _ tAmount) (Envelope eId eName eAmount) =
    let amount = (fromIntegral eAmount) + percentage * (fromIntegral tAmount) + (fromIntegral rAmount)
    in  Envelope eId eName (floor amount)


timeRuleFromSql :: [SqlValue] -> TimeRule
timeRuleFromSql (amount:frequency:start:[]) =
    TimeRule (fromSql amount) (fromSql frequency) (fromSql start)


categoryRuleFromSql :: [SqlValue] -> CategoryRule
categoryRuleFromSql (percentage:rAmount:start:tAmount:[]) =
    CategoryRule (fromSql percentage) (fromSql rAmount) (fromSql start) (fromSql tAmount)


envelopeSum :: [Envelope] -> Integer
envelopeSum = sum . map (\(Envelope _ _ amount) -> amount)


calcEnvelopeBalance :: [SqlValue] -> Maybe Envelope
calcEnvelopeBalance (eId:eName:eAmount:tAmount:[]) =
    case (safeFromSql eId, safeFromSql eName, safeFromSql eAmount, safeFromSql tAmount) of
        (Right eIdConverted, Right eNameConverted, Right eAmountConverted, Right tAmountConverted) ->
            Just (Envelope eIdConverted eNameConverted (eAmountConverted - tAmountConverted))
        (Right eIdConverted, Right eNameConverted, Right eAmountConverted, _) ->
            Just (Envelope eIdConverted eNameConverted eAmountConverted)
        otherwise -> Nothing
