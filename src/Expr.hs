module Expr where

data Expr = Val Int | Div Expr Expr deriving Show

eval :: Expr -> Maybe Int
-- eval (Val n)   = Just n
-- eval (Div x y) = case eval x of
--                     Nothing -> Nothing
--                     Just n -> case eval y of
--                                 Nothing -> Nothing
--                                 Just m  -> n `safeDiv` m
-- eval (Val n) = return n
-- eval (Div x y) = eval x >>= (\n ->
--                  eval y >>= (\m -> n `safeDiv` m))

eval (Val n) = return n
eval (Div x y) = do n <- eval x
                    m <- eval y
                    n `safeDiv` m

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv n m = Just (n `div` m)
