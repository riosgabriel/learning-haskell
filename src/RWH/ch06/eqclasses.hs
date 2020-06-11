class BasicEq a where
    isEqual, isNotEqual :: a -> a -> Bool

    isEqual x y = not (isNotEqual x y)

    isNotEqual x y = not (isEqual x y)

instance BasicEq Bool where
    isEqual True  True  = True
    isEqual False False = True
    isEqual _     _     = False

instance BasicEq Color where
    isEqual Red Red = True
    isEqual Green Green = True
    isEqual Blue Blue = True
    isEqual _ _ = False