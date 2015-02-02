module PutJson where

import SimpleJson
import Data.List (intercalate)

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
