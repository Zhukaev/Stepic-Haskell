module Test where

--Step 1

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs | n > 0 = helperplus n xs []
			| n < 0 = helperminus (abs n) xs []
			| n == 0 = xs
			
helperplus :: Int -> [a] -> [a] -> [a]
helperplus n [] ys = helperplus (mod n (length ys)) ys []
helperplus 0 xs ys = xs ++ ys
helperplus n (x:xs) ys = helperplus (n - 1) (xs) (ys ++ [x])


helperminus :: Int -> [a] -> [a] -> [a]
helperminus n [] ys = helperminus (mod n (length ys)) ys []
helperminus 0 xs ys = ys ++ xs
helperminus n xs ys = helperminus (n - 1) (init xs) ((last xs) : ys)

-- Step 3

arg1 arg2 = arg2
arg2 arg3 = arg3
arg3 arg4 = arg4
arg4 = True