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
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs)
  | xs == [] = [x]
  | x == head xs = compress xs
  | otherwise = x:(compress xs)

compress' :: (Eq a) => [a] -> [a]
compress' xs = foldr (\x acc -> if x == (head acc) then acc else x:acc) [last xs] xs
-- compress' = foldr (\x acc -> if x == head acc  then acc else x:acc) [] 

compress'' :: (Eq a) => [a] -> [a]
compress'' xs = reverse $ foldl (\acc x -> if x == (head acc) then acc else x:acc) [head xs] xs

compress''' :: (Eq a) => [a] -> [a]
compress''' [] = []
compress''' (x:xs) = x : ( compress''' $ dropWhile (==x) xs )

 -- 9 Problem 9
-- (**) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.

pack :: (Eq a) => [a] -> [[a]]
pack = foldr fun []
  where fun y [] = [[y]]
        fun y xx@(xs:xss) 
          | y == head xs = (y : xs) : xss
          | otherwise = [y] : xx

pack' :: (Eq a) => [a] -> [[a]]
pack' [] = []
pack' (x:xs) = (x : matching) : pack' rest
  where (matching, rest) = span (==x) xs

l = [1,1,1,1,2,2,2,2,3,3,3,3]
pack' l

pack'' :: (Eq a) => [a] -> [[a]]
pack'' = foldr fun []
  where 
      fun x [] = [[x]]
      fun x acc@(hs:ts) = if x == head hs
                             then (x: hs) : ts
                             else [x] : acc

-- additional
packEach :: [a] -> [[a]]
packEach [] = []
packEach [x] = [[x]]
packEach (x:y:xs) = (x:y:[]) : packEach xs

 -- 10 Problem 10
 -- (*) Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.  
 
encode :: [[a]] -> [(Int, a)]
encode = map (\xs -> (length xs, head xs))

encode' :: [[a]] -> [(Int, a)]
encode' xss = [(length xs, head xs) | xs <- xss]

l2 = [1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3]
encode $ pack l2
encode' $ pack l2

encode'' :: (Eq a) => [a] -> [(Int, a)]
encode'' = foldr fun []
  where 
      fun x [] = [(1, x)]
      fun x a@(first:acc) = if x == val
                               then (num + 1, val) : acc
                               else (1,x) : a
                                 where (num, val) = first

encode'' l2

encode''' :: [[a]] -> [(Int, a)]
encode''' = foldr (\x acc -> (length x, head x) : acc) []

encode''' $ pack l2

 -- 11 Problem 11
 -- (*) Modified run-length encoding.
 -- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists. 
 
 encode_mod :: [[a]] -> [(Int, a)]
 encode_mod = filter (\(count, _) -> count > 1) . encode 

l3 = [0,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3]

 encode_mod $ pack l3

encode_mod' :: [[a]] -> [(Int, a)]
encode_mod' = foldr (\x@(num, val) acc -> if num > 1 then x:acc else acc) [] . encode

encode_mod' $ pack l3

encode_mod'' :: [[a]] -> [(Int, a)]
encode_mod'' xs = let encoded = encode xs in [x | x@(num, val) <- encoded , num > 1] 

encode_mod'' $ pack l3
