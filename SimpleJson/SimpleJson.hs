module SimpleJson
(
    JValue(..)
    ,   getString
    ,   getBool
    ,   getDouble
    ,   getInt
    ,   getObject
    ,   getArray
    ,   isNull
) where

import Data.Maybe

data JValue = JString String
            | JBool Bool
            | JNumber Double
            | JNull 
            | JObject [(String, JValue)]
            | JArray [JValue]
            deriving (Eq, Show, Ord)

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool b) = Just b
getBool _         = Nothing

getDouble :: JValue -> Maybe Double
getDouble (JNumber n) = Just n
getDouble _           = Nothing

getInt :: JValue -> Maybe Int 
getInt (JNumber n) = Just (truncate n)
getInt _           = Nothing

getObject :: JValue -> Maybe [(String, JValue)]
getObject (JObject o) = Just o
getObject _           = Nothing

getArray :: JValue -> Maybe [JValue]
getArray (JArray a) = Just a
getArray _          = Nothing

isNull :: JValue -> Bool
isNull n = n == JNull
