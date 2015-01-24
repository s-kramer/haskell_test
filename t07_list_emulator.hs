data BinTree a = Node a (BinTree a) (BinTree a)
              | Empty
              deriving (Show)

-- Simple bin tree with 3 nodes
b1 = Node "parent node" (Node "empty node" Empty Empty) 
                        (Node "empty node" Empty Empty)

data List a = Cons a (List a)
            | Nil
            deriving (Show)

fromList(x:xs) = Cons x (fromList xs)
fromList([]) = Nil

toList(Cons x xs) = x:(toList xs)
toList(Nil) = []

-- Create a custom list
l1 = Nil
l2 = Cons 5 l1
l3 = Cons 17 l2
l4 = Cons 28 l3

--Convert it to standard list
converted = toList l4

-- Convert the std list back to custom
convertedAgain = fromList converted

data MyMaybe a = MyJust a
             | NotProvided
             deriving (Show)

data SingleConstructorList a = SCL a (MyMaybe (SingleConstructorList a) ) 
                                     (MyMaybe (SingleConstructorList a) )
                             deriving (Show)

scl0 = SCL 5 NotProvided NotProvided
scl1 = SCL 5 NotProvided (MyJust (SCL 17 NotProvided NotProvided))
