first :: (a,b,c) -> a
first (x, _, _) = x

second :: (a,b,c) -> b
second (_, x, _) = x

third :: (a,b,c) -> c
third (_, _, x) = x

first4 :: (a,b,c, d) -> a
first4 (x, _, _, _) = x

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' a = a:repeat' a

cycle' :: [a] -> [a]
cycle' xs = xs ++ cycle' xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' 1 x = [x]
replicate' n x 
  | n < 0 = error "Negative count provided!"
  | otherwise = x:replicate' (n-1) x

elem' :: (Eq a) => a -> [a] -> Bool
a `elem'` [] = False
a `elem'` (x:xs) = a == x || a `elem'` xs

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' [x] = [x]
quicksort' (x:xs) = 
    let prePivot = quicksort' [a | a <- xs, a < x]
        postPivot = quicksort' [a | a <- xs, a >= x]
    in prePivot ++ [x] ++ postPivot
