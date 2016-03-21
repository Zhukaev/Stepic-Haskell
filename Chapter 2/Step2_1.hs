--2.1.3

getSecondFrom :: t1 -> t2 -> t3 -> t2
getSecondFrom x y z = y 

--2.1.7

import Data.Function

multSecond = g `on` h

g x1 y1 = x1 * y1
h x2 = snd x2

--2.1.9

on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)