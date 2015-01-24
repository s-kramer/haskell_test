-- file t01_basics.hs

main = interact wordCount
    {- Lines count -}
    {- where wordCount input = show (length (lines input)) ++ "\n" -}
    {- words count -}
    {- where wordCount input = show (length (words input)) ++ "\n" -}
    {- char count -}
    where wordCount input = show (length input) ++ "\n"
