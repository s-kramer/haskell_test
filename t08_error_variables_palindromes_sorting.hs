import Data.List

mySecond xs = if null xs || null (tail xs)
    then error "List to short!"
    else head (tail xs)

safeSecond []  = Nothing
safeSecond xs  = if null (tail xs)
    then Nothing
    else Just (head (tail xs))

tidySecond :: [a] -> Maybe a
tidySecond (_:x:_) = Just x
tidySecond _ = Nothing

tidyPreLast :: [a] -> Maybe a
tidyPreLast (x:y:[]) = Just x
tidyPreLast (_:x:y:[]) = Just x
tidyPreLast _ = Nothing

-- Local variables 
lend amount balance = let reserve = 100
                          newBalance = balance - amount
                      in if amount > balance
                          then Nothing
                          else Just newBalance

lend1 amount balance = if amount > balance
                       then Nothing
                       else Just newBalance
    where reserve = 100
          newBalance = balance - amount
                        
-- Local functions + map introduction
pluralise :: String -> [Int] -> [String]
pluralise word count = map plural count
        where plural 0 = "No " ++ word ++ "s"
              plural 1 = "Single " ++ word
              plural n = show n ++ word ++ "s"

caseTest defval wraped = case wraped of
        Nothing -> defval
        Just value  -> value

data Fruit = Apple | Orange
apple = "apple"
orange = "orange"

exceptional :: String -> Fruit
exceptional fruit = case fruit of
        -- This raw strings may be replaces with guards
        "apple" -> Apple
        "orange" -> Orange

guardedLend :: Int -> Int -> Maybe Int
guardedLend amount balance
    | amount < 0        = Nothing
    | amount < balance  = Just newBalance
    | otherwise         = Nothing
   where reserve    = 100
         newBalance = balance - amount

lendGuarded amount balance
     | amount <= 0            = Nothing
     | amount > reserve * 0.5 = Nothing
     | otherwise              = Just newBalance
    where reserve    = 100
          newBalance = balance - amount            

-- myDrop with guards
myDrop :: Int -> [a] -> Maybe [a]
myDrop n xs | n <= 0 = Just xs
myDrop _ [] = Just []
myDrop n (_:xs) = myDrop (n-1) xs 

-- count list's lenght
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- count list's elements sum value
myListSum :: [Int] -> Int
myListSum [] = 0
myListSum (x:xs) = x + myListSum xs

-- count lists's elements mean value
-- Function's signature isn't yet writable
{- MyListMean :: [Int] -> Double -}
myListMean [] = 0
myListMean xs = (fromIntegral (myListSum xs)) / (fromIntegral (myLength xs))

-- Turn a list into palindrome
makePalindrome [] = []
makePalindrome (x:xs) = [x] ++ (makePalindrome xs) ++ [x]

-- Check if list is a palindrome
{- isPalindrome :: [a] -> Bool -}
isPalindrome [] = True
isPalindrome (x:[]) = True
isPalindrome (x:xs) = if x == last xs
                            then isPalindrome (init xs)
                            else False

-- Check if list is a palindrome (guarded version)
isPalindromeGuarded xs
    | length xs <= 1    = True
    | otherwise         = ((head xs) == (last xs)) && isPalindrome (init (tail xs))

sortLength xs ys 
    | length xs < length ys = LT
    | length xs > length ys = GT
    | otherwise             = EQ

--Sort list by sublist's length
sortBySubLength xss = sortBy sortLength xss

-- intersperse implementation
myIntersperseConcat _ [] = []
myIntersperseConcat _ (xs:[]) = xs
myIntersperseConcat separator (xs:xss) = xs ++ separator ++ (myIntersperseConcat separator xss)
