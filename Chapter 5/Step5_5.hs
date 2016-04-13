--5.5.3

main' :: IO ()
main' = do
	putStrLn "What is your name?"
	putStr "Name: "
	name <- getLine
	if name /= "" then putStrLn $ "Hi, " ++ name ++ "!" else main'