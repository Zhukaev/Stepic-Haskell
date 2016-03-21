--1.4.6

discount :: Double -> Double -> Double -> Double
discount limit proc sum = if sum >= limit then sum * (100 - proc) / 100 else sum

standardDiscount :: Double -> Double
standardDiscount = discount 1000 5

--1.4.9

import Data.Char
twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if isDigit x && isDigit y then digitToInt x * 10 + digitToInt y else 100

--1.4.11

dist :: (Double, Double) -> (Double, Double) -> Double
dist x y = sqrt ((fst x - fst y)^2 + (snd x - snd y)^2)