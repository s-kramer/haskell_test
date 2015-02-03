module PutJson where

import Data.List (intercalate)
import SimpleJson

renderJValue :: JValue -> String

renderJValue (JString s)   = show s
renderJValue (JNumber n)   = show n
renderJValue (JBool True)  = show "true"
renderJValue (JBool False) = show "false"
renderJValue JNull         = show "null"

renderJValue (JArray a) = "[" ++ value a ++ "]"
  where 
      value [] = ""
      value xs = intercalate ", " (map renderJValue xs)

renderJValue (JObject o) = "{" ++  pairs o ++ "}"
  where 
        pairs :: [(String, JValue)] -> String 
        pairs [] = ""
        pairs ps = intercalate ", " (map renderPair ps)
        renderPair (k,v) = show k ++ ": " ++ renderJValue v

putJValue :: JValue -> IO()
putJValue v = putStrLn (renderJValue v)
