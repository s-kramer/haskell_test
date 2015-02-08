module Vector where

data Vector a = Vector a a a deriving (Show)

v1 = Vector 1 2 3
v2 = Vector 4 5 6
v3 = Vector 3.0 2.8 7.1

vplus :: (Num a) => Vector a -> Vector a -> Vector a
vplus (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1+x2) (y1+y2) (z1+z2)

vmult :: (Num a) => Vector a -> a -> Vector a
vmult (Vector x1 y1 z1) m = Vector (x1*m) (y1*m) (z1*m)

vScalarMult :: (Num a) => Vector a -> Vector a -> a
vScalarMult (Vector x1 y1 z1) (Vector x2 y2 z2) = x1*x2 + y1*y2 + z1*z2


