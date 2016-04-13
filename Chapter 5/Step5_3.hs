--5.3.3

instance Functor SomeType where
    fmap f x = x >>= return . f