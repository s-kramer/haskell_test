isSentinel :: Char -> Bool
isSentinel x = x == '\n' || x == '\r'

lineSeparator :: String -> [String]
lineSeparator (x:xs) = if isSentinel x
                            then [x] ++ [lineSeparator xs]
                            else [x] ++ (lineSeparator xs)
