module Main where
import SimpleJson

main = print (JObject [("Foo", JNumber 1), ("Bar", JBool False)])
