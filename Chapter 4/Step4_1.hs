--4.1.5

instance Show Color where
    show Red = "Red"
    show Green = "Green"
    show Blue = "Blue"

	
--4.1.7

charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9

--4.1.8

data Color = Red | Green | Blue

stringToColor :: String -> Color
stringToColor "Red" = Red
stringToColor "Green" = Green
stringToColor "Blue" = Blue

--4.1.11

cmp :: LogLevel -> LogLevel -> Ordering
cmp Error Warning = GT
cmp Error Info = GT
cmp Warning Error = LT
cmp Warning Info = GT
cmp Info Warning = LT
cmp Info Error = LT
cmp _ _ = EQ

--4.1.13

processData :: SomeData -> String
processData smDt = case snd(doSomeWork smDt) of 
        0 -> "Success" 
        _ -> "Fail: " ++ (show (snd (doSomeWork smDt)))