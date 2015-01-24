myNot True = False
myNot False = True

sumList (x:xs) = x + sumList xs
sumList [] = 0

badExample (x:xs) = x + badExample xs

otherExample (x:xs) = x + otherExample xs
otherExample _ = 0

type CustomerID = Int
type CustomerName = String
type CustomerAddress = [String]

data Customer = Customer {
    customerID :: CustomerID,
    customerName :: CustomerName,
    customerAddress :: CustomerAddress
    } deriving (Show)


data MyMaybe a = MyJust a
            | Nothing
            deriving (Show)

data MyList a = Cons a (MyList a)
              | Nil
              deriving (Show)
