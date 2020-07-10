module FPCourse.MaybeMonadExample where

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs

-- head (tail (tail xs))

foo :: [a] -> Maybe a
foo xs =
    case safeTail xs of
        Nothing -> Nothing
        Just a -> case safeTail a of
                    Nothing -> Nothing
                    Just b  -> safeHead b

-- using bind directly
bar :: [a] -> Maybe a
bar xs =
    safeTail xs >>= \a ->
    safeTail a  >>= \b ->
    safeHead b

-- using do notation
baz :: [a] -> Maybe a
baz xs = do
    a <- safeTail xs
    b <- safeTail a
    safeHead b
