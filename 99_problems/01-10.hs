-- 1
last' :: [a] -> a
last' [] = error "Empty list provided"
last' [x] = x
last' (_:tail) = last'(tail)

--2
lastButOne :: [a] -> a
lastButOne [] = error "Too short list provided"
lastButOne [x] = error "Too short list provided"
lastButOne (x:_:[]) = x
lastButOne (x:xs) = lastButOne xs

lastButOne' :: [a] -> a
lastButOne' = last . init

--3 
element_at :: Int -> [a] -> a
element_at k (x:xs)
    | k < 0 = error "Incorrect index"
    | k == 0 = x
    | otherwise = element_at (k-1) xs

element_at' :: Int -> [a] -> a
element_at' k = head . drop k

--4
count' :: [a] -> Int
count' [] = 0
count' (x:xs) = 1 + count' xs

count'' :: [a] -> Int
count'' = foldl (\acc x -> 1 + acc) 0

count''' :: [a] -> Int
count''' = length

--5
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

reverse'' :: [a] -> [a]
reverse'' = foldr (\x acc -> acc ++ [x]) []

reverse''' :: [a] -> [a]
reverse''' = foldl (flip(:)) []

--6
isPalyndrome :: (Eq a) => [a] -> Bool
isPalyndrome [] = True
isPalyndrome [x] = True
isPalyndrome (x:xs)
  | x == last xs = isPalyndrome $ init xs
  | otherwise = False

--7
data NestedList a = Elem a | List [NestedList a]
  deriving (Show)
  
flatten :: NestedList a -> [a]
flatten (Elem n) = [n]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

--8
compress :: [a] -> [a]
compress = foldr (\x acc -> x:acc if x != head acc else acc) [] 

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs)
  | xs == [] = [x]
  | x == head xs = compress xs
  | otherwise = x:(compress xs)
