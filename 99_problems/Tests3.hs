module Tests3 where

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Enum, Bounded, Show, Eq, Read, Ord)

minDay = minBound :: Day
maxDay = maxBound :: Day
preSun = pred Sunday
postMon = succ Monday

weekend = [Saturday .. Sunday]
workWeek = [Monday .. Friday]

type Number = String
type Name = String
type PhoneBook = [(Name, Number)]
-- phoneBook :: [(String, String)]
phoneBook :: PhoneBook
phoneBook = 
    [("betty", "555-1231")
    ,("bonnie", "432-2341")
    ,("patsy", "392-1231")
    ,("lucille", "4112-1231")
    ,("wendy", "141-1239")
    ,("penny", "412-1231")]

isInPhoneBook :: Name -> Number -> PhoneBook -> Bool
isInPhoneBook  name phone book = (name, phone) `elem` book

infixr 5 :-:
data List a = Empty | a :-: (List a) deriving (Show, Eq, Ord, Read)

l1 = Empty
l2 = 5 :-: Empty
l3 = 17 :-: l2

numberMatcher :: (Num a, Eq a) => [a] -> a
numberMatcher (8:xs) = 8
numberMatcher (n:xs) = 5

data BinTree a = EmptyNode | Node a (BinTree a) (BinTree a) deriving (Show, Read, Eq)

firstSearchBinTree = Node 5 (Node 4 (Node 1 EmptyNode (Node 3 (Node 2 EmptyNode EmptyNode) EmptyNode)) EmptyNode) (Node 7 (Node 6 EmptyNode EmptyNode) (Node 9 (Node 8 EmptyNode EmptyNode) EmptyNode))

singleton :: a -> BinTree a
singleton a = Node a EmptyNode EmptyNode

isInBinTree :: (Ord a) => a -> BinTree a -> Bool
isInBinTree _ EmptyNode = False
isInBinTree n (Node x left right)
  | n == x = True
  | n < x = isInBinTree n left
  | n > x = isInBinTree n right

insertNode :: (Ord a) => a -> BinTree a -> BinTree a
insertNode n EmptyNode = singleton n
insertNode n (Node node bt1 bt2) 
  | n == node = Node n bt1 bt2
  | n < node = Node node (insertNode n bt1) bt2
  | n > node = Node node bt1 (insertNode n bt2)

createTree :: (Ord a) => [a] -> BinTree a
createTree = foldr nodeInserter EmptyNode 
  where 
      -- nodeInserter = insertNode
      nodeInserter y EmptyNode = singleton y
      nodeInserter y (Node x left right)
        | y == x = Node y left right
        | y > x = Node x left (nodeInserter y right)
        | y < x = Node x (nodeInserter y left) right

data TrafficLight = Red | Green | Yellow

-- class Equal a where
--     (===) :: a -> a -> Bool
--     (/==) :: a -> a -> Bool
--     a === b = not (a /== b)
--     a /== b = not (a === b)
    
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red light!"
    show Green = "Green light!"
    show Yellow = "Yellow light!"

class YesNo a where
    yesno :: a -> Bool

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno (Nothing) = False
instance YesNo (BinTree a) where
    yesno EmptyNode = False
    yesno Node{} = True

instance YesNo TrafficLight where
    yesno Red = False
    yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult
