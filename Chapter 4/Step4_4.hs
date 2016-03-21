--4.4.3

data Coord a = Coord a a

getcoordX (Coord x y) = x
getcoordY (Coord x y) = y

distance :: Coord Double -> Coord Double -> Double
distance point1 point2 = sqrt (((getcoordX point1) - (getcoordX point2))^2 + ((getcoordY point1) - (getcoordY point2))^2)

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance point1 point2 = abs ((getcoordX point1) - (getcoordX point2)) + abs ((getcoordY point1) - (getcoordY point2))

--4.4.6

import Data.Char(isDigit)

findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (x:xs)    | length xs == 0 && isDigit x == False = Nothing
                    | isDigit x == True = Just x
                    | otherwise = findDigit xs
					
--4.4.7

import Data.Char(isDigit)

findDigit :: [Char] -> Maybe Char

findDigitOrX :: [Char] -> Char
findDigitOrX x = (helper (findDigit x))

helper :: Maybe Char -> Char
helper Nothing = 'X'
helper (Just x) = x

--4.4.8

maybeToList :: Maybe a -> [a]
maybeToList (Just a) = [a]
maybeToList Nothing = []

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

--4.4.12

eitherToMaybe :: Either a b -> Maybe a
eitherToMaybe (Left a) = Just a
eitherToMaybe (Right _) = Nothing