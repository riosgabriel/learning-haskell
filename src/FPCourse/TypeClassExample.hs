module FPCourse.TypeClassExample where

data Foo = Bar | Baz

instance Show Foo where
    show Bar = "It's a Bar"
    show Baz = "It's a Baz"


data MySequence = A | B | C deriving (Show)

instance Enum MySequence where
    toEnum x = case x of 0 -> A ; 1 -> B ; 2 -> C
    fromEnum x = case x of A -> 0 ; B -> 1 ; C -> 2
    enumFrom     x   = enumFromTo     x maxBound
    enumFromThen x y = enumFromThenTo x y bound
        where
            bound | fromEnum y >= fromEnum x = maxBound
                  | otherwise                = minBound

instance Bounded MySequence where
    minBound = A
    maxBound = C
