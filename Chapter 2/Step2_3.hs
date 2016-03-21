--2.3.7

class Printable a where
	toString :: a -> String
	
instance Printable Bool where
	toString True = "true"
	toString False = "false"
instance Printable () where
	toString () = "unit type"
	
--2.3.9

instance (Printable a, Printable b) => Printable (a,b) where
	toString p = "(" ++ (toString (fst p))++ "," ++ (toString (snd p)) ++ ")"