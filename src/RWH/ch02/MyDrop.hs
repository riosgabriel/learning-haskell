-- file: ch02/myDrop.hs
myDrop :: Int -> [a] -> [a]
myDrop n xs = if n <= 0 || null xs
              then xs
              else myDrop (n - 1) (tail xs)

myDropGuard n xs
  | n <= 0 || null xs = xs
  | otherwise = myDropGuard (n - 1) (tail xs)

niceDrop n xs | n <= 0 = xs
niceDrop _ []          = []
niceDrop n (_:xs)      = niceDrop (n - 1) xs
              
lastButOne :: [a] -> a
lastButOne [] =  error "Not enough elements"
lastButOne (x:_:[]) = x
lastButOne xs = lastButOne(tail xs)
