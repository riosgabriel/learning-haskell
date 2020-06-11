import Data.Char (isSpace)
import Data.List (dropWhileEnd)

trim :: String -> String
trim = (dropWhile isSpace) . (dropWhileEnd isSpace)

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy f xs = first : splitBy f (dropWhile f rest)
               where (first, rest) = break f xs

splitByComma = splitBy (== ',')

toCoordinates xs = fmap toCoord (splitByComma ',' xs)
  where toCoord = (\x -> map id (trim x))

