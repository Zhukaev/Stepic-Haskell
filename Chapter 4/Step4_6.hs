--4.6.7

newtype Xor = Xor { getXor :: Bool }
    deriving (Eq,Show)

instance Monoid Xor where
	mempty = Xor False
	mappend (Xor a) (Xor b) = Xor (a /= b)

--4.6.9

newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)

instance Monoid a => Monoid (Maybe' a) where
  mempty = Maybe' (Just mempty)
  mappend (Maybe' Nothing) _ = Maybe' Nothing
  mappend _ (Maybe' Nothing) = Maybe' Nothing
  mappend (Maybe' a) (Maybe' b) = Maybe' (mappend a b)