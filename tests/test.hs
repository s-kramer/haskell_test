doubleSmallNumber x = if x > 100
                          then x
                          else x * 2

doubleSmallNumber' x = (if x > 100 then x else x * 2)

allTrianglesWithPerimeter :: Int -> [(Int, Int, Int)]
allTrianglesWithPerimeter p = [(a,b,c) | a<-[1..10], b<-[1..10], c<-[1..10], a+b+c == p, a^2+b^2==c^2]

lucky :: Int -> String
lucky 7 = "Lucky 7!"
lucky _ = "You're out of luck!"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
{- addVectors x y = (fst x + fst y, snd x + snd y) -}
addVectors (x1, x2) (y1, y2) = (x1 + y1, x2 + y2)

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length'(xs)

capital :: String -> String
capital "" = error "Empty string!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ show x

calcBMI :: (RealFloat a) => a -> a -> String
calcBMI weight height
  | bmi <= 18.5 = "Underweight"
  | bmi <= 25.0 = "Normal"
  | bmi <= 30.0 = "Fat"
  | otherwise = "Whale"
    where bmi = weight / height ^ 2

compare' :: (Ord a) => a -> a -> Ordering
compare' x y
  | x > y = GT
  | x == y = EQ
  | otherwise = LT


initials :: String -> String -> String
initials first last = [f] ++ "." ++ [l] ++ "."
  where f = head first
        l = head last
  {- where (f, l) = (head first, head last) -}
  {- where (f:_) = first -}
        {- (l:_) = last -}

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Empty list provided"
maximum' [x] = x
maximum' (x:xs) 
  | x > m = x
  | otherwise = m
    where m = maximum' xs

{- = max x (maximum xs) -}

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : (replicate' (n-1) x)

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' n [] = []
take' n (x:xs) = x:take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x] : []

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

-- sth wrong with this one
zip'' :: [a] -> [a] -> [[a]]
zip'' [] _ = []
zip'' _ [] = []
zip'' (x:xs) (y:ys) = [x,y] : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' y (x:xs) 
  | x == y    = True
  | otherwise = y `elem'` xs

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = smaller ++ [x] ++ bigger
  where smaller = quicksort' [a | a<-xs, a <= x]
        bigger  = quicksort' [a | a<-xs, a > x]

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f ( f x )

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

makeTuple :: a -> b -> (a,b)
makeTuple x y = (x,y)

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

flip'' :: (a -> b -> c) -> (b -> a -> c)
flip'' f = g
  where g x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x         = x : filter' f xs
  | otherwise   = filter' f xs

quicksort'' :: (Ord a) => [a] -> [a]
quicksort'' [] = []
quicksort'' (x:xs) = quicksort'' (filter' (<=x) xs) ++ [x] ++ quicksort'' (filter'(>x) xs)

largestNumber = let divisable x y = (x `mod` y) == 0 in last (filter (flip divisable 3829) [1..100000])

largestNumber' = head ( filter p [100000, 99999..])
  where p x = x `mod` 3829 ==0

weirdSum = sum (takeWhile p ( filter odd ( map (^2) [1..])))
  where p x = x < 10000
weirdSum' = sum (takeWhile (\x -> x < 10000) ( filter odd ( map (^2) [1..])))
weirdSum'' = sum . takeWhile (\x -> x < 10000) $ filter odd $ map (^2) [1..]
weirdSum''' = sum . takeWhile (\x -> x < 10000) . filter odd . map (^2) $ [1..]
weirdSum'''' = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

collatzChainsCount = length (filter p (map makeCollatz [1..100]))
  where p x = length x > 15

makeCollatz :: (Integral a, Eq a) => a -> [a]
makeCollatz 1 = [1]
makeCollatz n = n : makeCollatz x
  where x = makeSingleCollatz n

makeSingleCollatz :: (Integral a) => a -> a
makeSingleCollatz x
  | odd x = 3*x + 1
  | otherwise = x `div` 2

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

elem' :: (Eq a) => a -> [a] -> Bool
-- elem' e xs = foldl (\acc x -> x == e || acc) False xs
elem' e xs = foldl (\acc x -> if x == e then True else acc) False xs

map' :: (a -> a) -> [a] -> [a]
map' f xs = foldr (\x acc -> f x : acc) [] xs

max' :: (Ord a) => [a] -> a
max' xs = foldl1 (\x acc -> if x > acc then x else acc) xs

reverse' :: [a] -> [a]
-- reverse' = foldr (\x acc -> acc ++ [x]) [] 
-- reverse' = foldl (\acc x -> x : acc) [] 
reverse' = foldl (flip(:)) [] 

product' :: (Num a) => [a] -> a
product' = foldl1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x:acc else acc) []

head' :: [a] -> a
-- head' = foldl1 (\acc x -> acc)
head' = foldr1 (\x acc -> x)

last' :: [a] -> a
-- last' = foldr1 (\x acc -> acc)
last' = foldl1 (\acc x -> x)

rootsCount :: Int
rootsCount = length (takeWhile (\ys -> ys < 1000) (scanl (+) 0 (map (sqrt) [1..])))

sumOfThirdPowers :: Int
sumOfThirdPowers = sum . takeWhile (<10000) $ map (^3) [1..]

stock = [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)]  
stockOver1000 = head $ dropWhile (\(x,_,_,_) -> x < 1000) stock

elementCount :: (Ord a) => [a] -> [(a, Int)]
elementCount = map (\x -> (head x, length x)) . group . sort

searchSub :: (Eq a) => [a] -> [a] -> Bool
searchSub needle stack = let nlen = length needle in
    foldl (\acc x -> if take nlen x == needle then True else acc ) False $ tails stack


