data BinTree a = Node a (BinTree a) (BinTree a) 
               | Empty
               deriving (Show)

-- Count bin tree height
binTreeHeight (Empty) = 0
binTreeHeight (Node _ Empty Empty) = 1
binTreeHeight (Node _ binTreeA Empty) = 1 + binTreeHeight binTreeA
binTreeHeight (Node _ Empty binTreeB) = 1 + binTreeHeight binTreeB
binTreeHeight (Node _ binTreeA binTreeB) = 1 + max (binTreeHeight binTreeA) (binTreeHeight binTreeB)
