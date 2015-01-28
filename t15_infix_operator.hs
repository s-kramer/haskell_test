-- -- file: ch04/InteractWith.hs
-- -- Save this in a source file, e.g. Interact.hs

-- import System.Environment (getArgs)

-- interactWith function inputFile outputFile = do
--     input <- readFile inputFile
--     writeFile outputFile (function input)

-- main = mainWith myFunction
--   where mainWith function = do
--       args <- getArgs
--       case args of
--            [input,output] -> interactWith function input output
--            _ -> putStrLn "error: exactly two arguments needed"

--         -- replace "id" with the name of our function below
--         myFunction = 

a `plus` b = a + b
data a `Pair` b = a `Pair` b
  deriving (Show)

foo = Pair 1 2
bar = True `Pair` "quux"
