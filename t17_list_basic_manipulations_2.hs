import Data.Maybe
import Data.Char
import Data.List
import Data.Either

myReverse [] = []
myRevers [x] = [x]
myRevers xs = last xs : myRevers (init xs)

myAnd [] = True
myAnd (False:_) = False
myAnd (True:xs) = myAnd xs

myOr [] = False
myOr (x:xs) = x || myAnd xs

myAll _ [] = True
myAll pred (x:xs) = pred x && myAll pred xs

myAny _ [] = False
myAny pred (x:xs) = pred x || myAny pred xs

myTake 0 _ = []
myTake _ [] = []
myTake n (x:xs) = x: myTake (n-1) xs

myDrop _ [] = []
myDrop 0 xs = xs
myDrop n (x:xs) = myDrop (n-1) xs

-- mySplitAt _ [] = ([], [])
-- mySplitAt 0 xs = ([], xs)
mySplitAt n xs = (take n xs, drop n xs)

myTakeWhile _ [] = []
myTakeWhile pred (x:xs) 
  | pred x = x : myTakeWhile pred xs
  | otherwise = []

myDropWhile _ [] = []
myDropWhile pred (x:xs) 
  | pred x = myDropWhile pred xs
  | otherwise = x:xs

mySpan pred xs = (myTakeWhile pred xs, myDropWhile pred xs)
myBreak pred xs = (myTakeWhile (not . pred) xs, myDropWhile (not . pred) xs)

myElem _ [] = False
myElem y (x:xs) 
  | x == y = True
  | otherwise = myElem y xs

myNotElem _ [] = True
myNotElem y (x:xs) 
  | x == y = False
  | otherwise = myNotElem y xs

myFilter _ [] = []
myFilter pred (x:xs)
  | pred x = x : myFilter pred xs
  | otherwise = myFilter pred xs

myIsPrefixOf [] _ = True
myIsPrefixOf ys xs 
  | ys == take (length ys) xs = True
  | otherwise = False

-- myIsPostfixOf [] _ = True
myIsPostfixOf ys xs 
  | ys == drop (length xs - length ys) xs = True
  | otherwise = False

myIsInfixOf [] _ = True
myIsInfixOf _ [] = False
myIsInfixOf ys (x:xs)
  | ys == take (length ys) (x:xs) = True
  | otherwise = myIsInfixOf ys xs

myZip [] []  = []
myZip [] (y:ys) = []
myZip (x:xs) [] = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys

myZipWith pred xs ys 
  | null xs || null ys = []
  | otherwise = value : myZipWith pred (tail xs) (tail ys)
    where value = pred (head xs) (head ys)

myZipWithVars :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWithVars pred xs ys 
  | null xs || null ys = []
  | otherwise = value : myZipWith pred tx ty
    where 
        (befx, tx) = splitAt 1 xs
        (befy, ty) = splitAt 1 ys
        hx = head befx
        hy = head befy
        value = pred hx hy

mySafeHead :: [a] -> Maybe a
mySafeHead [] = Nothing
mySafeHead (x:xs) = Just x

mySafeTail :: [a] -> Maybe [a]
mySafeTail [] = Nothing
mySafeTail (x:xs) = Just xs

mySafeLast :: [a] -> Maybe a
mySafeLast [] = Nothing
mySafeLast [x] = Just x
mySafeLast (x:xs) = mySafeLast xs

mySafeInit :: [a] -> Maybe [a]
mySafeInit [] = Nothing
mySafeInit [x] = Just []
mySafeInit (x:xs) = Just (x : fromMaybe [] (mySafeInit xs))

-- Working version
splitWithExternal _ [] = []
splitWithExternal _ [x] = [[x]]
splitWithExternal pred (x:xs)
  | not (pred x) = [x] : current : rest
  | otherwise = (x : current) : rest
    where current : rest = splitWithExternal pred xs

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
-- splitWith _ [x] = [x] : []
splitWith _ [x] = [[x]]
splitWith pred (x:xs)
  | pred x = (x : current) : back
  | otherwise = [x] : current : back
    where current : back = splitWith pred xs

myMap :: (a -> b) -> [a] -> [b]
myMap f (x:xs) = f x : myMap f xs
myMap _ _ = []

myFilter2 :: (a -> Bool) -> [a] -> [a]
myFilter2 pred (x:xs) 
  | pred x = x : myFilter pred xs
  | otherwise = myFilter pred xs
myFilter2 _ _ = []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f zero (x:xs) = myFoldl f (f zero x) xs
myFoldl _ zero [] = zero

myFilter3 :: (a -> Bool) -> [a] -> [a]
-- myFilter3 pred xs = foldr step [] xs
myFilter3 pred = foldr step []
  where step x ys | pred x = x : ys
                  | otherwise = ys

myAsInt :: String -> Int
myAsInt ('-':xs) = -myAsInt xs
myAsInt xs 
  | null xs = error "No number provided"
  | otherwise = foldl' step 0 xs
  where 
      step :: Int -> Char -> Int
      step y '.' = error "Only integers are supported"
      step y x 
        | temp < y = error "Overflow"
        | otherwise = temp
          where temp = y * 10 + (ord x - ord '0')

myAsIntSecond :: String -> Either String Int
myAsIntSecond ('-':xs) = Right (-myAsInt xs)
myAsIntSecond xs 
  | null xs = Left "No number provided"
  | any (`notElem` ['1'..'9']) xs = Left "Non-digit found!"
  | otherwise = Right (foldl' step 0 xs)
  where 
      step :: Int -> Char -> Int
      -- step y '.' = error "Only integers are supported"
      step y x 
        | temp < y = error "Overflow"
        | otherwise = temp
          where temp = y * 10 + (ord x - ord '0')

myConcatFoldr :: [[a]] -> [a]
myConcatFoldr [] = []
myConcatFoldr xss = foldr step [] xss
  where step ys yss = ys ++ yss


myTakeWhileRecurs :: (a -> Bool) -> [a] -> [a]
myTakeWhileRecurs _ [] = []
myTakeWhileRecurs pred (x:xs) 
  | pred x = x : myTakeWhileRecurs pred xs
  | otherwise = []

myTakeWhileFoldr :: (a -> Bool) -> [a] -> [a] 
myTakeWhileFoldr pred = foldr step []
  where 
      step y ys
        | pred y = y : ys
        | otherwise = []

-- takeWhileFoldr p = foldr step []
--   where 
--       step x ys 
--         | p x = x : ys
--         | otherwise = []

myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
-- myGroupBy _ [] = []
myGroupBy pred = foldr step []
  where 
      -- step :: (b -> [b] -> [b])
      step x [] = [[x]]
      step x (y:ys)
        | pred x (head y) = (x : y) : ys
        | otherwise = [x] : rest
          where rest = y:ys
-- {{{
{-|
foldr (+) 0 [1,1,5,5] 
    == 1 +                foldr (+) 0 [1,5,5]
    == 1 + (1 +           foldr (+) 0 [5,5])
    == 1 + (1 + (5 +      foldr (+) 0 [5]))
    == 1 + (1 + (5 + (5 + foldr (+) 0 [])))
    == 1 + (1 + (5 + (5 + 0)))

foldr f s [1,1,5,5] 
    == 1 f                foldr f s [1,5,5]
    == 1 f (1 f           foldr f s [5,5])
    == 1 f (1 f (5 f      foldr f s [5]))
    == 1 f (1 f (5 f (5 f foldr f s [])))
    == 1 f (1 f (5 f (5 f s)))

    f == step, s = []
    == 1 f (1 f (5 : [5]))

-}
-- }}}

jfgroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
jfgroupBy f = foldr step []
  where 
      step i a | null a = [[i]]
        | f i (head (head a)) = (i : head a) : tail a
        | otherwise = [i] : a
-- {{{
{-|
jfgroupBy f [1,1,2,3] == 
    foldr step [] [1,1,2,3]
    == 1 step                           foldr step [] [1,2,3]
    == 1 step (1 step                   foldr step [] [2,3])
    == 1 step (1 step (2 step           foldr step [] [3]))
    == 1 step (1 step (2 step (3 step   foldr step [] [])
    == 1 step (1 step (2 step (3 step   [])
    == 1 step (1 step (2 step ([[3]])
    == 1 step (1 step ([2] : [[3]])
    == 1 step ([1] : [2] : [[3]]
    == (1 : [1]) : [2] : [[3]]

-}
-- }}}

mySplitWith :: (a -> Bool) -> [a] -> [[a]]
mySplitWith _ [] = []
mySplitWith _ [x] = [[x]]
mySplitWith pred (x:xs)
  | pred x = (x : current) : rest
  | otherwise = [x] : current : rest
    where 
        current:rest = mySplitWith pred xs

myAnyFold :: (a -> Bool) -> [a] -> Bool
myAnyFold pred xs = or (foldr step [] xs)
  where 
      -- step x [] = pred x : [False]
      step x ys = pred x : ys

{-|
foldl f s [1,2,3,4]
 == foldl f (f 1 s) [2,3,4]
 == foldl f (f 2 (f 1 s)) [3,4]
 == foldl f (f 3 (f 2 (f 1 s))) [4]
 == foldl f (f 4 (f 3 (f 2 (f 1 s)))) []
 ==          f 4 (f 3 (f 2 (f 1 s)))
-}

myCycleFold :: [a] -> [a]
myCycleFold xs = foldl step [] xs
  where step x ys 
