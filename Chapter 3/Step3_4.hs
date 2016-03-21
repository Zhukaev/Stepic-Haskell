--3.4.3

concatList :: [[a]] -> [a]
concatList xs = foldr (++) [] xs

--3.4.5

lengthList :: [a] -> Int
lengthList = foldr (\x s -> s + 1) 0

--3.4.6

sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if odd x == True then s + x else s) 0