-- file: ch04/InteractWith.hs
-- Save this in a source file, e.g. Interact.hs

import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        myFunction = fixLines

lineSplit :: String -> [String]
lineSplit cs = let (pre,rest) = break isSentinel cs
                in pre: case rest of
                             ('\r':'\n':rest)   -> lineSplit rest
                             ('\n':rest)     -> lineSplit rest
                             ('\r':rest)     -> lineSplit rest
                             _               -> []

isSentinel x = x == '\n' || x == '\r'

fixLines :: String -> String
fixLines input = unlines (lineSplit input)
