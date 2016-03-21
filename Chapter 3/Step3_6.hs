--3.6.3

lastElem :: [a] -> a
lastElem = foldl1 (\x s -> s)