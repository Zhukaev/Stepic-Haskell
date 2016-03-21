--3.2.3

import Data.Char

readDigits :: String -> (String, String)
readDigits arr = helper "" "" arr

helper :: String -> String -> [Char] -> (String, String)	
helper words numbers x	| length x > 0 = if (isDigit (head x)) == True then (helper (words) (numbers ++ (head x : [])) (tail x)) else (numbers,x)
			| length x == 0 = (numbers,words)

--3.2.4

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj first second arr = helper first second arr []

helper first second arr newarr	| length arr == 0 = newarr
				| (first) (head arr) == True = helper first second (tail arr) (newarr ++ (head arr) : [])
				| (second) (head arr) == True = helper first second (tail arr) (newarr ++ (head arr) : [])
				| otherwise = helper first second (tail arr) (newarr)
				
--3.2.5

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort (filter (< x) xs) ++ [x] ++ qsort (filter (>= x) xs)

--3.2.7

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes arr = helper arr []

helper arr snc	| length arr == 0 = snc
		| otherwise = helper (tail arr) (snc ++ ((head arr) * (head arr) : []) ++ ((head arr) * (head arr) * (head arr) : []))
		
--3.2.8

perms :: [a] -> [[a]]
perms []     = [[]]
perms [x]    = [[x]]
perms (x:xs) = ins x `concatMap` perms xs
  where
    ins :: a -> [a] -> [[a]]
    ins x y = map (\p -> (take p y) ++ [x] ++ (drop p y)) [0..(length y)]
	
--3.2.10

import Data.Char

delAllUpper :: String -> String
delAllUpper str = helper (words str) []

helper :: [String] -> [String] -> String
helper [] newstr = unwords (filter (not . null) newstr)
helper str1 newstr = helper (tail str1) (newstr ++ isLow (head str1) : [])

isLow :: String -> String
isLow str = if any isLower str == True then str else []

--3.2.12

max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 x y z = helper x y z []

helper [] [] [] newarr = newarr
helper (x:xs) (y:ys) (z:zs) newarr = helper xs ys zs (newarr ++ (max (max x y) z) : [])