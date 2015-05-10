 -- 3 Problem 13
-- (**) Run-length encoding of a list (direct solution).
-- Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X. 
data Counted a = Single a | Multi (Int, a)  
  deriving Show  

l3 = [0,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3]  

encodeDirect :: (Eq a) => [a] -> [Counted a]
encodeDirect [] = []
encodeDirect (x:xs) = elem : encodeDirect other
  where 
      (same, other) = span (==x) xs
      l = length same
      elem = if l == 0 then Single x else Multi ((l + 1), x)

encodeDirect l3

-- 4 Problem 14
-- (*) Duplicate the elements of a list. 
duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = x:x:duplicate xs

duplicate' :: [a] -> [a]
duplicate' = foldr (\x acc -> x:x:acc) []

duplicate'' :: [a] -> [a]
duplicate'' = reverse . foldl (\acc x -> x:x:acc) []
duplicate'' = foldl (\acc x -> acc ++ [x,x]) []

l4 = [1..5]
duplicate' l4
duplicate'' l4

duplicate''' :: [a] -> [a]
duplicate''' = concatMap(replicate 2)
duplicate''' l4

-- 5 Problem 15
-- (**) Replicate the elements of a list a given number of times. 
duplicateN :: Int -> [a] -> [a]
duplicateN _ [] = []
duplicateN n (x:xs) = (replicate n x) ++ duplicateN n xs

duplicateN' :: Int -> [a] -> [a]
duplicateN' n = foldr (\x acc -> replicate n x ++ acc) []

duplicateN'' :: Int -> [a] -> [a]
duplicateN'' n = concatMap (\x -> replicate n x)
duplicateN'' = concatMap . replicate

duplicateN 3 l4
duplicateN' 3 l4
duplicateN'' 4 l4

-- 6 Problem 16
-- (**) Drop every N'th element from a list. 
dropN :: Int -> [a] -> [a]
dropN n xs = concat . map (init) $ splitEvery n xs
  where 
      splitEvery _ [] = []
      splitEvery n xs = first : splitEvery n rest
          where (first, rest) = splitAt n xs

dropN' :: Int -> [a] -> [a]
dropN' _ [] = []
dropN' n xs = take (n-1) xs ++ (dropN' n $ drop n xs)

dropN 3 [1..20]
dropN' 3 [1..20]

dropN'' :: Int -> [a] -> [a]
dropN'' n xs = map snd . filter (\x -> fst x /= n) $ zip [1..] xs
todo: other solutions
