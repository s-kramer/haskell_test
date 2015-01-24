mySecond xs = if null xs || null (tail xs)
    then error "List to short!"
    else head (tail xs)
