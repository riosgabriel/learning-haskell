data List a = Cons a (List a) | Nil deriving (Show)

fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

converseFromList (Cons x xs) = x : converseFromList xs
converseFromList Nil = []