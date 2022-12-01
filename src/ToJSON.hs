module ToJSON where

import HSON (HSON, Key, Value (Array, Boolean, Null, Number, Object, String))

------------------------- Writing Keys ---------------------------------------

-- | Converts a HSON key to a string for writing to the JSON file
keyToString :: String -> String
keyToString s = "\"" ++ s ++ "\"" ++ ":"

------------------------- Writing Values ---------------------------------------

-- | Converts an HSON value to a string
valueToString :: Value -> String
valueToString x = case x of
  String s -> stringToString s
  Number n -> numberToString n
  Boolean b -> booleanToString b
  Array a -> arrayToString a
  Object o -> objectToString o
  Null -> nullToString

-- | Converts a HSON string value to a string for writing to the JSON file
stringToString :: String -> String
stringToString s = s ++ ","

-- | Converts a HSON number value to a string for writing to the JSON file
numberToString :: Double -> String
numberToString = undefined

-- | Converts a HSON boolean value to a string for writing to the JSON file
booleanToString :: Bool -> String
booleanToString b = if b then "true" else "false"

-- | Converts a HSON array value to a string for writing to the JSON file
arrayToString :: [Value] -> String
arrayToString = undefined

-- | Converts a HSON object value to a string for writing to the JSON file
objectToString :: HSON -> String
objectToString = undefined

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
toJSON = undefined
