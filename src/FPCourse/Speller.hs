module FPCourse.Speller where

speller :: [String] -> String
speller [] = ""
speller [x] = "and " ++ spell x
speller (x:xs) = spell x ++ ", " ++ speller xs

spell :: String -> String
spell word@(x:_) = x : " is for " ++ word
