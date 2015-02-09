collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n 
  | even n = n : collatz (n `div` 2)
  | otherwise = n : collatz (3*n+1)

collatzSeq = [y | x <- [1..100], let y = collatz x]

shortCollatzSeqs = length (filter (<=15) (map length collatzSeq))
longCollatzSeqs = length (filter (>15) (map length collatzSeq))


longCollatzSeqsLambda = length (filter (\ xs -> length xs > 15) (map collatz [1..100]))

take' :: (Integral n) => n -> [a] -> [a]
take' _ [] = []
take' 0 _ = []
take' n (x:xs) = x : take' (n-1) xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' a = foldl (\acc x -> ((x == a) || acc)) False

-- sum' :: (Num a) => [a] -> a
-- sum' = foldl (+) 0

repeat' :: [a] -> [a]
repeat' = foldr (:) [] 

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []  

-- How many elements does it take for the sum of the roots of all natural numbers to exceed 1000? 
rb1000ec = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..])))
rb1000ec2 = length . takeWhile (<1000) . scanl1 (+) $ map sqrt [1..]

takeUntilNot :: (a -> Bool) -> [a] -> [a]
takeUntilNot _ [] = []
takeUntilNot pred (x:xs) 
  | not (pred x) = x : takeUntilNot pred xs
  | otherwise = []

oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
oddSquareSum2 = sum . takeWhile (<10000) . filter odd $ map (^2) [1..]
