module NNHP.Lists where

-- Find the last element of a list.
myLast :: [a] -> a
myLast []       = error "No end for empty list"
myLast [x     ] = x
myLast (_ : xs) = myLast xs

--  Find the last but one element of a list. 
myButLast :: [a] -> a
myButLast []       = error "No end for empty list"
myButLast [x]      = error "Only one item"
myButLast [x, _]   = x
myButLast (_ : xs) = myButLast xs

 -- Find the K'th element of a list. 
 -- The first element in the list is number 1. 
elementAt :: [a] -> Int -> a
elementAt (x : _ ) 1 = x
elementAt (_ : xs) k = elementAt xs (k - 1)
elementAt _        _ = error "index out of bounds"

-- Find the number of elements of a list. 
myLength :: [a] -> Int
myLength = foldl (\acc _ -> acc + 1) 0


-- Reverse a list.
myReverse :: [a] -> [a]
-- myReverse = foldl (\acc x -> x : acc) []
myReverse = foldl (flip (:)) []

--Find out whether a list is a palindrome. 
--A palindrome can be read forward or backward; e.g. (x a m a x). 
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome xs = xs == myReverse xs

f :: Double -> String
f c = if (let t = c * 9 / 5 in t + 32) <= (-459.67)
    then "really cold"
    else "It is not that cold"

doSomething x | x < 3     = report "less than three"
              | otherwise = report "normal"
    where report str = "the input is " ++ str

absolute x | x < 0     = -x
           | otherwise = x

return42 :: IO () -> Int
return42 _ = 42

myPrint str = return42 (print str)
