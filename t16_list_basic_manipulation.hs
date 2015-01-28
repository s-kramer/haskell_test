myLength [] = 0
myLength (x:xs) = 1 + myLength xs

-- l1 = myLength []
-- l2 = myLength [1..5]
-- l3 = myLength "some long string"

myNull [] = True
myNull _ = False

myHead [] = 0
myHead (x:xs) = x
