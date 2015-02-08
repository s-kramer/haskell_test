-- module Tests where
    
import Data.List
import Data.Char
import Data.Function
import qualified Data.Map as Map

elementsCount :: (Ord a) => [a] -> [(Int, a)]
elementsCount = map (\xs -> (length xs, head xs)) . group . sort

contains' :: (Eq a) => [a] -> [a] -> Bool
contains' needle heystack = foldl isNeedle False (tails heystack)
  where nlen = length needle
        isNeedle acc x = if take nlen x == needle then True else acc

words' :: [Char] -> [[Char]] 
words' str = filter (not . all isSpace) . groupBy ((==) `on` isSpace) $ str

caesar :: Int -> [Char] -> [Char]
caesar shift msg= 
    let ords = map ord msg
        shifted = map (+ shift) ords
     in map chr shifted

caesar' :: Int -> [Char] -> [Char]
caesar' shift = map (chr . (+shift) . ord) 

decaesar :: Int -> String -> String
decaesar shift = map (chr . (subtract shift) .ord)

phoneBook = [("patty", "555-4923")
            ,("herb", "123-2342")
            ,("blah", "333-6666")
            ,("some1", "332-2341")]

findKey :: (Eq a) => [a] -> [([a], [t])] -> Maybe ([a],[t])
findKey key = find (\x -> let (k,v) = x in key == k)

findKey' :: (Eq a) => [a] -> [([a], [t])] -> Maybe ([a],[t])
findKey' key = foldr (\y xs -> let (k,v) = y in if k == key then Just y else Nothing) Nothing

findValueByKeyUnsafe :: (Eq a) => [a] -> [([a], [t])] -> [t]
-- If value won't be found this will produce runtime error
findValueByKeyUnsafe key = snd . head . filter (\x -> let (k,v) = x in k == key)

findKey'' :: (Eq a) => a -> [(a,t)] -> Maybe t
findKey'' key = lookup key

nums = [1..10]
chars = ['a'..'z']
vals = zip nums chars

data Car = Car { company :: String, model :: String, year :: Int} deriving Show
