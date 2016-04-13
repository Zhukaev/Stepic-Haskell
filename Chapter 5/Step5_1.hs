--5.1.3

instance Functor Point3D where
    fmap f (Point3D x y z) = (Point3D (f x) (f y) (f z))
	
--5.1.4

instance Functor GeomPrimitive where
    fmap f (Point(Point3D x y z)) = Point(Point3D (f x) (f y) (f z))
    fmap f (LineSegment (Point3D x1 y1 z1) (Point3D x2 y2 z2)) = LineSegment(Point3D (f x1) (f y1) (f z1)) (Point3D (f x2) (f y2) (f z2))
	
--5.1.6

instance Functor Tree where
    fmap _ (Leaf (Nothing)) = Leaf (Nothing)
    fmap f (Leaf (Just a)) = Leaf (Just (f a))
    fmap f (Branch (a) (Just b) (c)) = Branch (fmap f a) (Just (f b)) (fmap f c)
    fmap f (Branch (a) (Nothing) (c)) = Branch (fmap f a) (Nothing) (fmap f c)
	
--5.1.8

instance Functor (Entry k1 k2) where
    fmap f (Entry (k1, k2) v) = Entry (k1, k2) (f v)

instance Functor (Map k1 k2) where
     fmap f (Map entries) = Map (map (fmap f) entries)