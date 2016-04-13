--5.6.8

local' :: (r -> r') -> Reader r' a -> Reader r a
local' f m = Reader ((runReader m) . f)

--5.6.9

usersWithBadPasswords :: Reader UsersTable [User]
usersWithBadPasswords = asks $ map fst . filter (("123456" ==) .snd)