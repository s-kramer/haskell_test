module Problems1_10 where 

import Data.List
-- 1
myLast :: [a] -> a
myLast [] = error "Empty list!"
myLast [x] = x
myLast (x:xs) = myLast xs

-- 2
myLastButOne :: [a] -> a
myLastButOne = last . init

-- 3
elementAt :: [a] -> Int -> a
elementAt [] _ = error "Empty list!"
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n-1)

--4
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

--5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse xs = last xs : myReverse (init xs)

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

--6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) = (x == last xs) && isPalindrome (init xs)

--7
data NestedList a = Elem a | List [NestedList a]
  deriving (Show)

flattenList :: NestedList a -> [a]
flattenList (Elem a) = [a]
flattenList (List (x:xs)) = flattenList x ++ flattenList (List xs)
flattenList (List []) = []

-- flattenListFoldr :: [NestedList a] -> [a]
flattenListFoldr (Elem a) = [a]
flattenListFoldr (List xs) = foldr (++) [] $ map flattenListFoldr xs

--8
compress :: (Eq a) => [a] -> [a]
compress = foldr parseElem [] 
  where 
      parseElem y xs 
        | null xs = [y]
        | y == head xs = xs
        | otherwise = y : xs

compress2 :: (Eq a) => [a] -> [a]
compress2 [] = []
compress2 (x:xs) = x : (compress2 $ dropWhile (==x) xs)

compress3 :: (Eq a) => [a] -> [a]
compress3 (x:ys@(y:_)) 
  | x == y = compress3 ys
  | otherwise = x : compress3 ys
compress3 y = y

--9
pack :: (Eq a) => [a] -> [[a]]
pack = Data.List.group

pack2 :: (Eq a) => [a] -> [[a]]
pack2 = foldr handleElem []
  where 
      handleElem y [] = [[y]]
      handleElem y (xs:xss)
        | y == head xs = (y : xs) :xss
        | otherwise = [y] : (xs:xss)

pack3 :: (Eq a) => [a] -> [[a]]
pack3 [] = []
pack3 (x:xs) = (takeWhile (==x) xs) : (pack3 $ dropWhile (==x) xs)

pack4 :: (Eq a) => [a] -> [[a]]
pack4 [] = []
pack4 xx@(x:xs) = currentRange : pack4 rest
    where 
        (currentRange, rest) = span (==x) xx 

--10
--
makePair :: [a] -> (Int, a)
makePair xs = (length xs, head xs)

makePairs :: [[a]] -> [(Int, a)]
makePairs [] = []
makePairs (x:xs) = (makePair x) : (makePairs xs)

encode :: (Eq a) => [a] -> [(Int, a)]
encode = makePairs . pack 

encode2 :: (Eq a) => [a] -> [(Int, a)]
encode2 xs = map makePair $ pack xs

encode3 :: (Eq a) => [a] -> [(Int, a)]
encode3 = foldr handleMakingPair [] . pack
  where handleMakingPair y xs = makePair y : xs

encode4 :: (Eq a) => [a] -> [(Int, a)]
encode4 = map (\xs -> (length xs, head xs)) . pack

encode5 :: (Eq a) => [a] -> [(Int, a)]
encode5 xs = [(length x, head x) | x <- pack xs]
