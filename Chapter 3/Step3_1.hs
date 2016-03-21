--3.1.3

addTwoElements :: a -> a -> [a] -> [a]
addTwoElements x y z = x : y : z 

--3.1.4

z = []

nTimes:: a -> Int -> [a]
nTimes x y = nTime x y z

nTime:: a -> Int -> [a] -> [a]
nTime x y z		| y > 0 = nTime (x) (y - 1) (x : z)
				| otherwise = z
				
--3.1.8

oddsOnly :: Integral a => [a] -> [a]
oddsOnly x = filter (odd) x

--3.1.10

isPalindrome :: Eq a => [a] -> Bool
isPalindrome x	| length x == 0 = True
				| length x == 1 = True
				| head x == last x = isPalindrome (tail (init x))
				| otherwise = False
				
--3.1.12

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3	(a:as)	(b:bs)	(c:cs) 	= (a+b+c) : sum3 as bs cs
sum3	(a:as)	(b:bs)	_		= (a+b) : sum3 as bs []
sum3	_		(b:bs)	(c:cs)	= (b+c) : sum3 [] bs cs
sum3	(a:as)	_		(c:cs)	= (a+c) : sum3 as [] cs
sum3	(a:as)	_		_		= (a) : sum3 as [] []
sum3	_		(b:bs)	_		= (b) : sum3 [] bs []
sum3	_		_		(c:cs)	= (c) : sum3 [] [] cs
sum3 	_ 		_		_		= []

--3.1.13

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems arr = helper arr [[]] (head arr)

helper :: Eq a => [a] -> [[a]] -> a -> [[a]]
helper [] newarr _ = newarr
helper (x:xs) newarr newelem = helper (xs) (if x == newelem then (init newarr) ++ [(x : (last newarr))] else  newarr ++ [[x]]) (x) 