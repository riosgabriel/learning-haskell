module FPCourse.OnlyFunctions where

-- if-then-else in terms of function
if' :: Bool -> a -> a -> a
if' True x _  = x
if' False _ y = y

-- list in terms of function
cons = \x xs -> (\c -> c x xs)
empty = \f -> True

mkTup = \x y z -> \t -> t x y z

tp = mkTup 1 "2" [3]

fst' tp = tp (\x y z -> x)
snd' tp = tp (\x y z -> z)

