import Problems1_10

--11
data Encode_t a = Single a | Multiple Int a
  deriving (Show)
encodeModified :: (Eq a) => [a] -> [Encode_t a]
encodeModified xs = map (makeEncode) (pack xs)
  where 
      makeEncode :: (Eq a) => [a] -> (Encode_t a)
      makeEncode xs 
        | (length xs) /= 1 = Multiple (length xs) (head xs)
        | otherwise = Single (head xs)

encodeModified2 :: (Eq a) => [a] -> [Encode_t a]
encodeModified2 = map encodeHelper . encode
    where encodeHelper (1,c) = Single c
          encodeHelper (n,c) = Multiple n c

--12
decode :: [Encode_t a] -> [a]
decode [] = []
decode (Single x:xs) = x : decode xs
decode ((Multiple n c):xs) = (replicate n c) ++ decode xs

decode2 :: [Encode_t a] -> [a]
decode2 = concatMap decodeHelper 
  where decodeHelper (Single c) = [c]
        decodeHelper (Multiple n c) = replicate n c

--13
encodeModified3 :: (Eq a) => [a] -> [Encode_t a]
encodeModified3 [] = []
encodeModified3 xx@(x:xs) 
  | len > 1 = (Multiple len x) : encodeModified3 end
  | otherwise = Single x : encodeModified3 end 
  where 
      len = length begin
      (begin, end) = span (==x) xx

--14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

dupli2 :: [a] -> [a]
dupli2 = foldr (\y xs -> y:y:xs) [] 
