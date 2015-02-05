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
flattenList (Elem x : List xs) = (x : flattenList xs)
