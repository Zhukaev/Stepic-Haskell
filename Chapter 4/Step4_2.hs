--4.2.3

data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

--4.2.5

data Shape = Circle Double | Rectangle Double Double

area :: Shape -> Double
area (Circle r) = pi * r^2
area (Rectangle x y) = x * y

--4.2.6

data Result' = Fail' Int | Success'

instance Show Result' where
    show Success' = "Success"
    show (Fail' err) = "Fail: " ++ (show err)

doSomeWork' :: SomeData -> Result'
doSomeWork' smDt = case snd(doSomeWork smDt) of 
        0 -> Success' 
        err -> Fail' err
		
--4.2.8

data Shape = Circle Double | Rectangle Double Double

square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare (Circle r) = False
isSquare (Rectangle a b) = if a == b then True else False