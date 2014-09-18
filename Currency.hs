module Currency (formatCurrency) where

import Data.List


groupN :: Integer -> [a] -> [[a]]
groupN _ [] = []
groupN 0 _ = []
groupN n xs = genericTake n xs : groupN n (genericDrop n xs)


intersperseL :: Integer -> a -> [a] -> [a]
intersperseL n c [] = []
intersperseL n c xs = intercalate [c] $ groupN n xs


intersperseR :: Integer -> a -> [a] -> [a]
intersperseR n c = reverse . intersperseL n c . reverse


formatCurrency :: Integer -> String
formatCurrency n =
    let f = formatDecimal . show . abs $ n in
    if n < 0 then "-$" ++ f else '$' : f


formatDecimal :: String -> String
formatDecimal s =
    let p = pad 3 '0' s in
    case genericSplitAt (length p - 2) p of
        (a, b) -> intersperseR 3 ',' a ++ "." ++ b


pad :: Integer -> a -> [a] -> [a]
pad n c s = genericReplicate (n - genericLength s) c ++ s
