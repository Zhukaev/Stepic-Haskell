--3.5.8

meanList :: [Double] -> Double
meanList arr = (someFun arr) / (fromIntegral (length arr) :: Double )

someFun :: [Double] -> Double
someFun = foldr (\x s -> s + x) 0

--3.5.9

evenOnly :: [a] -> [a]
evenOnly arr = helper1 arr []

helper1 :: [a] -> [a] -> [a]
helper1 [] newarr = newarr
helper1 (x:xs) newarr = helper2 xs newarr


helper2 :: [a] -> [a] -> [a]
helper2 [] newarr = newarr
helper2 (x:xs) newarr = helper1 xs (newarr ++ x : [])

--3.5.10

evenOnly :: [a] -> [a]
evenOnly arr = fst $ unzip (filter (\x -> even $ snd x) (zip arr [1..]))