--1.6.6

seqA :: Integer -> Integer
seqA n = go n (1,2,3)
  where
    go n (a1, a2, a3)  | n == 0 = a1
					| n == 1 = a2
					| n == 2 = a3
					| otherwise =  go (n-1) (a2, a3, a3 + a2 - 2 * a1)
					
--1.6.8

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x	| x == 0 = (0, 1) 
		| otherwise = go x (0, 0)
	where	
		go x (summ, count)	| x == 0 = (summ, count)
					| x > 0 = go (quot x 10) (summ + mod x 10, count + 1)
					| x < 0 = go (quot x 10) (summ + (10 - mod x 10), count + 1)
					
--1.6.9

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = helper f a b 0
	where helper f a b res
					| (abs (b - a)) < step = res + (((f b) + (f a)) / 2 * (b - a))
					| a < b           = helper f (a + step) b (res + (((f a) + (f (a + step))) / 2 * step))
					| a > b           = helper f (a - step) b (res - (((f a) + (f (a - step))) / 2 * step))
					where step = 0.0001