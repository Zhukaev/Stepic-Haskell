--1.5.4

doubleFact :: Integer -> Integer
doubleFact n = if n <= 1 then 1 else n * doubleFact (n - 2)

--1.5.8

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci (-1) = 1
fibonacci n | n > 0 = fibonacci (n - 1) + fibonacci (n - 2)
fibonacci n | n < 0 = fibonacci (n + 2) - fibonacci (n + 1)

--1.5.10

fibonacci :: Integer -> Integer
fibonacci n = go n (0,1)
  where
    go n (a, b) | n==0      = a
                | n > 0 = go (n-1) (b, a+b)
				| n < 0 = go (n+1) (b, a-b)