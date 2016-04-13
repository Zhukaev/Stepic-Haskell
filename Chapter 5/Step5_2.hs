--5.2.3

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg x = Log [msg] (f x)

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = Log (fst ++ snd) c
	where 
		(Log fst b) = f x
		(Log snd c) = g b
		
--5.2.5

returnLog :: a -> Log a
returnLog = Log []

--5.2.7

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log msga a) f = Log (msga ++ msgb) b where
	(Log msgb b) = f a

--5.2.8

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList = foldl (>>=) . return