{- lastButOne xs = head (drop ((length xs)-2) xs) -}

{- lastButOne xs = if length xs < 2 -}
    {- then error "Too short list" -}
    {- else last (take ((length xs)-1) xs) -}

{- lastButOne :: [a] -> a -}
{- lastButOne = last . init -}

{- lastButOne xs = lastButOneImpl xs 0 -}
{- lastButOneImpl xs n = if n == 1 -}
    {- then last xs -}
    {- else lastButOneImpl (take ((length xs)-1) xs) (n+1) -}

lastButOne xs = last( init xs)
