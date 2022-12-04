module ToJSON (integerToString, hsonToString, keyToString, valueToString, stringToString, numberToString, booleanToString, arrayToString, objectToString, nullToString, toJSON) where

import Control.Concurrent (yield)
import Data.Char
import Data.Map qualified as Map
import HSON (HSON, Key, Value (Array, Boolean, Integer, Null, Number, Object, String))
import Test.QuickCheck.Text (number)

------------------------- Writing Keys ---------------------------------------

-- | Converts a HSON key to a string for writing to the JSON file
keyToString :: String -> String
keyToString s = "\"" ++ s ++ "\"" ++ ": "

-- >>> keyToString "abc"
-- "\"abc\": "

------------------------- Writing Values ---------------------------------------

-- | Converts an HSON value to a string
valueToString :: Value -> String
valueToString x = case x of
  String s -> stringToString s
  Number n -> numberToString n
  Integer i -> integerToString i
  Boolean b -> booleanToString b
  Array a -> arrayToString a
  Object o -> objectToString o
  Null -> nullToString

-- | Converts a HSON string value to a string for writing to the JSON file
stringToString :: String -> String
stringToString s = "\"" ++ s ++ "\""

-- | Converts a HSON number value to a string for writing to the JSON file
numberToString :: Double -> String
numberToString = show

-- | Converts a HSON number value to a string for writing to the JSON file
integerToString :: Int -> String
integerToString = show

-- | Converts a HSON boolean value to a string for writing to the JSON file
booleanToString :: Bool -> String
booleanToString b = if b then "true" else "false"

-- | Converts a HSON array value to a string for writing to the JSON file
arrayToString :: [Value] -> String
arrayToString a = "[" ++ arrayToStringHelper a ++ "]"
  where
    arrayToStringHelper :: [Value] -> String
    arrayToStringHelper [] = ""
    arrayToStringHelper [x] = valueToString x
    arrayToStringHelper (x : xs) = valueToString x ++ ", " ++ arrayToStringHelper xs

-- | Converts a HSON object value to a string for writing to the JSON file
objectToString :: HSON -> String
objectToString lst = "{" ++ objectToStringHelper lst ++ "}"
  where
    objectToStringHelper :: [(Key, Value)] -> String
    objectToStringHelper [] = ""
    objectToStringHelper [(k, v)] = keyToString k ++ valueToString v
    objectToStringHelper ((k, v) : xs) = keyToString k ++ valueToString v ++ ",\n" ++ objectToStringHelper xs

-- | Converts a HSON null value to a string for writing to the JSON file
nullToString :: String
nullToString = "null"

------------------------- Converting HSON to String ----------------------------

-- | Converts a HSON object to a string
hsonToString :: HSON -> String
hsonToString = objectToString

------------------------- Creating JSON File  ----------------------------------

-- | creates a new JSON file from an HSON object
toJSON :: FilePath -> HSON -> IO ()
toJSON fp h = writeFile fp (hsonToString h)
