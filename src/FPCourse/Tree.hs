module FPCourse.Tree where

data Tree = Leaf
            | Node Int Tree Tree
            deriving Show

treeDepth :: Tree -> Int
treeDepth Leaf = 0
treeDepth (Node _ leftTree rightTree) =
    1 + max (treeDepth leftTree) (treeDepth rightTree)

treeSum :: Tree -> Int
treeSum Leaf = 0
treeSum (Node value leftTree rightTree) =
    value + treeSum leftTree + treeSum rightTree

isSortedTree :: Tree -> Int -> Int -> Bool
isSortedTree Leaf _ _ = True
isSortedTree (Node x leftTree rightTree) minVal maxVal =
    let leftSorted  = isSortedTree leftTree minVal x
        rightSorted = isSortedTree rightTree x maxVal
    in  x >= minVal && x < maxVal && leftSorted && rightSorted

addNewMax :: Tree -> Tree
addNewMax Leaf             = Node 0 Leaf Leaf
addNewMax (Node x t1 Leaf) = Node x t1 (Node (x + 1) Leaf Leaf)
addNewMax (Node x t1 t2  ) = Node x t1 (addNewMax t2)

toList :: Tree -> [Int]
toList Leaf                = []
toList (Node x left right) = x : toList left ++ toList right

insert :: Tree -> Int -> Tree
insert Leaf new = Node new Leaf Leaf
insert (Node value left right) new
    | new < value = Node value (insert left new) right
    | otherwise   = Node value left (insert right new)
