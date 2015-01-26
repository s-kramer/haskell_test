lineSplit :: String -> [String]
lineSplit cs = let (pre,rest) = break isSentinel cs
{- lineSplit cs = let (pre:rest:[]) = break isSentinel cs -}
                in pre: case rest of
                    (rest:"\n")     -> lineSplit rest
                    (rest:"\r")     -> lineSplit rest
                    (rest:"\r\n")   -> lineSplit rest
                    _               -> []

isSentinel x = x == '\n' || x == '\r'

