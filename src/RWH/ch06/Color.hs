data Color = Red | Green | Blue
        deriving (Read, Show, Eq, Ord)

colorEq :: Color -> Color -> Bool
colorEq Red Red = True
colorEq Green Green = True
colorEq Blue Blue = True
colorEq _ _ = False