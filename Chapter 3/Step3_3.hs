--3.3.3

fibStream :: [Integer]
fibStream = 0 : 1 : zipWith (+) fibStream (tail fibStream)

--3.3.5

repeatHelper = (id) 

