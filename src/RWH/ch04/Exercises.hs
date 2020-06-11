import Data.Char

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail [x] = Nothing
safeTail (_:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_:xs) = Just (head (reverse xs))

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith p xs = splitWithAcc p xs []
                 where splitWithAcc _ [] acc = acc
                       splitWithAcc p (x:xs) acc =
                         if not(p x) then splitWithAcc p xs ([x] : acc)
                         else splitWithAcc p xs acc

splitWithFoldl :: (a -> Bool) -> [a] -> [[a]]
splitWithFoldl p xs = foldl step [] xs
                      where step acc x | not (p x) = [x]: acc
                                       | otherwise = acc

asIntFold :: String -> Integer
asIntFold "" = error "Empty String"
asIntFold xs = foldl step 0 xs
  where step acc x | isDigit x = acc * 10 + toInteger (digitToInt x)
                   | otherwise = error "not a digit"

type ErrorMessage = String
type ParseResult = Either ErrorMessage Int
asIntEither :: String -> ParseResult
asIntEither xs = foldl step (Right 0) xs
  where step :: ParseResult -> Char -> ParseResult
        step acc x | isDigit x = fmap(\a -> 10 * a + digitToInt x) acc
                   | otherwise = Left ("not a digit '" ++ [x] ++ "'")

concat' :: [[a]] -> [a]
concat' xss = foldr step [] xss
  where step xs acc = acc ++ xs

concatFoldr :: [[a]] -> [a]
concatFoldr xs = foldr (++) [] xs

type TakeWhile a = (a -> Bool) -> [a] -> [a]

takeWhileRec :: TakeWhile a
takeWhileRec f xs = step f xs []
  where step f (x:xs) acc | f x = step f xs (acc ++ [x])
                          | otherwise = acc

takeWhileFoldr :: TakeWhile a
takeWhileFoldr f xs = foldr step [] xs
  where step x acc | f x = x : acc
                   | otherwise = acc

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy f xs = foldr step [[]] xs
  where step x [[]] = [[x]]
        step x (ys@(y:_):yss) | f x y = (x:ys):yss
                              | otherwise = [x]:ys:yss

anyFold :: (a -> Bool) -> [a] -> Bool
anyFold f [] = False
anyFold f xs = foldl step False xs
  where step acc x | f x = True
                   | otherwise = acc

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) =
  qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a <= x]
    larger  = [b | b <- xs, b > x]

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) = if a == x then True else elem' a xs

nth :: [a] -> Int -> a
nth (x:_) 0 = x
nth [] _ = error "index not found"
nth (_:xs) n  | n < 1 = error "negative index"
              | otherwise = nth xs (n - 1)

ack :: [Int] -> [Int] -> [Int]
ack [] ys = 1 : ys
ack (x:xs) [] = ack xs [1]
ack (x:xs) (y:ys) = ack xs (ack (x:xs) ys)
