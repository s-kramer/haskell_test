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
