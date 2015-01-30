myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myNull [] = True
myNull _ = False

myHead [] = error "Empty list exception!"
myHead (x:xs) = x

myTail [] = error "Empty list exception!"
myTail (x:xs) = xs

myLast [] = error "Empty list exception!"
myLast (x:[]) = x
myLast (x:xs) = myLast xs

myInit [] = error "Empty list exception!"
myInit (x:[]) = []
-- myInit (x:xs) = [x] ++ myInit xs
myInit (x:xs) = x : myInit xs

headNonEmptyFirst xs = if not (myNull xs)
    then myHead xs
    else 'Z'

headNonEmptySecond [] = 'Z'
headNonEmptySecond (x:_) = x

myListJoiner (x:xs) ys = x: myListJoiner xs ys
myListJoiner [] (y:ys) = y: myListJoiner [] ys
myListJoiner [] [] = []

myConcat [] = []
myConcat (xs:xxs) = xs ++ myConcat xxs

myReverse [] = []
-- myReverse (x:xs) = myReverse xs ++ [x]
myReverse (x:xs) = ((myReverse xs) : x)


