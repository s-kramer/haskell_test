lineSplit :: String -> [String]
lineSplit cs = let (pre,rest) = break isSentinel cs
                in pre: case rest of
                             ('\r':'\n':rest)   -> lineSplit rest
                             ('\n':rest)     -> lineSplit rest
                             ('\r':rest)     -> lineSplit rest
                             _               -> []

isSentinel x = x == '\n' || x == '\r'
