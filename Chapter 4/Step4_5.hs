--4.5.3

data List a = Nil | Cons a (List a) deriving Show

fromList :: List a -> [a]
fromList Nil = []
fromList (Cons x xs) = x:fromList xs

toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Cons x(toList xs)

--4.5.4

data Nat = Zero | Suc Nat

fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

toNat :: Integer -> Nat
toNat 0 = Zero
toNat n = Suc (toNat (n - 1))

add :: Nat -> Nat -> Nat
add Zero Zero = Zero
add Zero n = n
add n Zero = n
add n (Suc m) = add (Suc n) m 

mul :: Nat -> Nat -> Nat
mul Zero n = Zero
mul n Zero = Zero
mul n m = toNat((fromNat n) * (fromNat m))

fac :: Nat -> Nat 
fac n = toNat(factorial (fromNat n) 1)

factorial :: Integer -> Integer -> Integer
factorial 1 fact = fact
factorial 0 fact = 1
factorial n fact = factorial (n - 1) (fact * n)

--4.5.5

data Tree a = Leaf a | Node (Tree a) (Tree a)

height :: Tree a -> Int
height (Leaf a) = 0
height (Node a b) = 1 + max (height a) (height b)

size :: Tree a -> Int
size (Leaf a) = 1
size (Node a b) = 1 + size a + size b

--4.5.6

data Tree a = Leaf a | Node (Tree a) (Tree a)

avg :: Tree Int -> Int
avg t =
    let (c,s) = go t
    in s `div` c
  where
    go :: Tree Int -> (Int,Int)
    go (Leaf x) = (1, x)
    go (Node left right) = let
        (lc, ls) = go left
        (rc, rs) = go right
      in (lc + rc, ls + rs)