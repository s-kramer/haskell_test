import Data.Maybe

data JValue = JString String
            | JBool Bool
            | JNumber Double
            | JNull 
            | JObject [(String, JValue)]
            | JArray [JValue]
            deriving (Eq, Show, Ord)

fromJString :: JValue -> Maybe String
fromJString (JString s) = Just s
fromJString _ = Nothing
