import Data.List

len :: [a] -> Int -> Int
len (_:xs) acc = len xs (acc + 1)
len [] acc = acc

len2 :: [a] -> Int
len2 [] = 0
len2 (_:xs) = 1 + len2 xs

len3 :: [a] -> Int
len3 xs = lenAcc xs 0
    where lenAcc [] acc = acc
          lenAcc (_:xs) acc = lenAcc xs (acc + 1)

mean :: (Fractional a) => [a] -> a
mean xs = meanInner xs 0 0
    where meanInner [] sum len = sum / fromIntegral len
          meanInner (x:xs) sum len = meanInner xs (sum + x) (len + 1)

makePalindrome :: [a] -> [a]
makePalindrome xs = xs ++ reverse xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome xs = xs == reverse xs

sortByLength :: [[a]] -> [[a]]
sortByLength l = sortBy (\a b -> compare (length a) (length b)) l
