--5.7.3

evalWriter :: Writer w a -> a
evalWriter = fst . runWriter