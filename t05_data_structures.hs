data BookInfo = Book Int String [String]
                deriving (Eq, Show)

data MagazineInfo = MagazineInfo Int String [String]
                deriving (Show)

myInfo = Book 123456789 "Some title" ["first author", "second author"]

type CustomerID = Int
type Review = String

data BookReview = BookReview BookInfo CustomerID Review

type CardHolder = String
type CardNumber = String
type Address = [String]

data BilingInfo = CreditCard CardNumber CardHolder Address
                | CashOnDelivery
                | Invoice CustomerID
                deriving (Show)



