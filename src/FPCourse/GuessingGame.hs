module FPCourse.GuessingGame where

check :: String -> String -> Char -> (Bool, String)
check word display c =
    (c `elem` word, 
        [if x == c then c else y | (x, y) <- zip word display])

turn :: String -> String -> Int -> IO ()
turn word display n 
    | n == 0 = putStrLn "You Lose"
    | word == display = putStrLn "You Win"
    | otherwise = mkguess word display n

starman :: String -> Int -> IO ()
starman word = turn word ['_' | x <- word]

mkguess :: String -> String -> Int -> IO ()
mkguess word display n = do 
    putStrLn (display ++ "  " ++ replicate n '*')
    putStr "Enter your guess: "
    q <- getLine
    let (correct, display') = check word display $ head q
    let n' = if correct then n else n-1
    turn word display' n'

-- A real improvement to the game would be to 
-- generate a random word, perhaps from a list of words 
-- or a dictionary file. If you are feeling ambitious, 
-- you might try this. It would involve generating a random number 
-- `i` and read in the ith word from a dictionary. You might import 
-- System.Random and use a Haskell random number generator.