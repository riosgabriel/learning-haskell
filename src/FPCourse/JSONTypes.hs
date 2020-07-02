module FPCourse.JSONTypes (
        JValue(..),
        mkJPair,
        mkJObj
         )
    where

import           Data.Map hiding (map)

type JMap = Data.Map.Map String JValue
data JValue = JString String
            | JNumber Integer
            | JObject JMap
            | JArray [JValue]
            | JBool Bool
            | JNull
    deriving (Show)

mkJPair :: String -> JValue -> JValue
mkJPair k v = JObject (Data.Map.singleton k v)

mkJObj :: [JValue] -> JValue
mkJObj jVals =
    let
        listOfMaps = map (\(JObject pair) -> pair) jVals
        combinedMap = Data.Map.unions listOfMaps
    in
        JObject combinedMap

f :: a -> b
g :: b -> c
v :: a

v = h f g

