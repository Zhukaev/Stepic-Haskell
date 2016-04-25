data OddCont a = Singl a | BiCons a a (OddCont a) 
                 deriving (Eq,Show)
--step 5

lenOC :: OddCont a -> Int
lenOC (Singl a) = 1
lenOC (BiCons _ _ c) = 2 + lenOC c

countConsOC :: OddCont a -> Int
countConsOC (Singl a) = 1
countConsOC (BiCons _ _ c) = 1 + countConsOC c 

--step 6

headOC :: OddCont a -> a
headOC (BiCons a _ _) = a
headOC (Singl a) = a

lastOC :: OddCont a -> a
lastOC (Singl a) = a
lastOC (BiCons _ _ c) = lastOC c

--step 7

sumOC :: Num a => OddCont a -> a
sumOC(Singl a) = a
sumOC(BiCons a b c) = a + b + (sumOC c)

findOC :: (a -> Bool) -> OddCont a -> Maybe a
findOC f (Singl a) 	| (f a) = Just a
					| otherwise = Nothing
findOC f (BiCons a b c)	| (f a) = Just a
						| (f b) = Just b
						| otherwise = findOC f c
		
--step 8
		
takeNCons :: Int -> OddCont a -> OddCont a
takeNCons n _ | n <= 0 = error "Only positive n allowed!"
takeNCons n (Singl a) = Singl a
takeNCons 1 (BiCons a b c) = Singl a
takeNCons n (BiCons a b c) = BiCons a b (takeNCons (n - 1) c)

--step 9

instance Functor OddCont where
	fmap f (Singl a) = Singl (f a)
	fmap f (BiCons a b c) = BiCons (f a) (f b) (fmap f c)
	
--step 10
	
foldrOC :: (a -> b -> b) -> b -> OddCont a -> b
foldrOC f num (Singl a) = f a num
foldrOC f num (BiCons a b c) = f a (f b (foldrOC f num c))

--step 11

concat3OC :: OddCont a -> OddCont a -> OddCont a -> OddCont a
concat3OC a b c = helper2 a (helper1 (lastOC a) b c)

helper1 last1 b c	| ifsin b = BiCons last1 (headOC b) c
					| otherwise = BiCons last1 (headOC b) (helper1 (sndB b)(unpack b) c)

ifsin (Singl a) = True
ifsin (BiCons _ _ _) = False

sndB (BiCons a b c) = b
sndB (Singl a) = undefined

unpack (BiCons a b c) = c
unpack (Singl a) = undefined

helper2 (Singl a) d = d
helper2 (BiCons a b c) d = BiCons a b (helper2 c d)

--step12

concatOC :: OddCont (OddCont a) -> OddCont a
concatOC (Singl a) = a
concatOC (BiCons a b c) = concat3OC a b (concatOC c)

--step 13

instance Monad OddCont where
  return x = Singl x
  x >>= f = concatOC (fmap f x)