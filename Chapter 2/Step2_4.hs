--2.4.3

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab x
        |doesEnrageGork x == True && doesEnrageMork x == True = stomp (stab x)
        |doesEnrageGork x == False && doesEnrageMork x == True = stomp x
        |doesEnrageGork x == True && doesEnrageMork x == False = stab  x
        |doesEnrageGork x == False && doesEnrageMork x == False =  x
		
--2.4.5

a=127.2
b=24.12
c=0.1
d=2

--2.4.7

class (Bounded a, Enum a, Eq a) => SafeEnum a where
  ssucc :: a -> a
  ssucc x
      |x == maxBound = minBound
      |otherwise = succ x
  spred :: a -> a
  spred x
      |x == minBound = maxBound
      |otherwise = pred x
	  
--2.4.9

avg :: Int -> Int -> Int -> Double
avg x y z = (fromInteger (toInteger x + toInteger y + toInteger z)) / 3