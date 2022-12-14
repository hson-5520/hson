module FromJSONSchema where

import Control.Exception
import Data.Either
import Data.List qualified
import Data.Map
import Data.Map qualified as Map
import Data.Maybe
import Data.Maybe qualified as Maybe
import FromJSON
import HSON
  ( HSON (H),
    Key,
    Value (Array, Boolean, Integer, Null, Number, Object, String),
  )
import HSONSchema
  ( ArrProperties (..),
    BoolProperties (..),
    HSONSchema (..),
    IntProperties (..),
    NumProperties (..),
    ObjProperties (..),
    StrProperties (..),
  )
import Parser qualified as P
import Test.HUnit
import ToJSON

-------------------------------- Helpers ---------------------------------------

-- | looks up a key in a map and parses the corresponding value as an int
matchInt :: String -> String -> Map Key Value -> Either String (Maybe Int)
matchInt err s map =
  case Map.lookup s map of
    Just (Integer y) -> Right $ Just y
    Just _ -> Left err
    Nothing -> Right Nothing

-- | looks up a key in a map and parses the corresponding value as a number
matchNumber :: String -> String -> Map Key Value -> Either String (Maybe Double)
matchNumber err s map =
  case Map.lookup s map of
    Just (Number y) -> Right $ Just y
    Just (Integer x) -> Right $ Just (fromIntegral x)
    Just _ -> Left err
    Nothing -> Right Nothing

-- | looks up a key in a map and parses the corresponding value as a bool
matchBool :: String -> String -> Map Key Value -> Either String (Maybe Bool)
matchBool err s map =
  case Map.lookup s map of
    Just (Boolean y) -> Right $ Just y
    Just _ -> Left err
    Nothing -> Right Nothing

-- | looks up a key in a map and parses the corresponding value as a string
matchString :: String -> String -> Map Key Value -> Either String (Maybe String)
matchString err s map =
  case Map.lookup s map of
    Just (String y) -> Right (Just y)
    Just _ -> Left err
    Nothing -> Right Nothing

-- | filters a list to return only the integers in it
filterIntArray :: [Value] -> [Int]
filterIntArray = Prelude.foldr combHelper []
  where
    combHelper x acc = case x of
      Integer y -> y : acc
      _ -> acc

-- | filters a list to return only the numbers in it
filterNumberArray :: [Value] -> [Double]
filterNumberArray = Prelude.foldr combHelper []
  where
    combHelper x acc = case x of
      Number y -> y : acc
      _ -> acc

-- | filters a list to return only the booleans in it
filterBoolArray :: [Value] -> [Bool]
filterBoolArray = Prelude.foldr combHelper []
  where
    combHelper x acc = case x of
      Boolean y -> y : acc
      _ -> acc

-- | filters a list to return only the strings in it
filterStringArray :: [Value] -> [String]
filterStringArray = Prelude.foldr combHelper []
  where
    combHelper x acc = case x of
      String y -> y : acc
      _ -> acc

-- | checks if all the elements in a list are of the same type
isArrayConsistent :: [Value] -> Bool
isArrayConsistent x =
  length x == length (filterStringArray x)
    || length x == length (filterIntArray x)
    || length x == length (filterNumberArray x)
    || length x == length (filterBoolArray x)

-- | checks if all the boolean elements in a list are of the same value
-- | and returns that value if that is the case
boolArrayHelper :: [Bool] -> Maybe Bool
boolArrayHelper x
  | Prelude.null x = Nothing
  | all (== True) x = Just True
  | all (== False) x = Just False
  | otherwise = Nothing

------------------------- HSON to HSON Schema  ---------------------------------

-- | parses an HSON object into an HSONSchema
-- | based on the argument of the "type" key
schemaParser :: HSON -> Either String HSONSchema
schemaParser (H lst) =
  let map = Map.fromList lst
   in do
        str <- matchString "type is not a string" "type" map
        case str of
          Nothing -> Left ""
          Just "number" -> numberHelper (H lst)
          Just "integer" -> intHelper (H lst)
          Just "boolean" -> boolHelper (H lst)
          Just "array" -> arrHelper (H lst)
          Just "object" -> objHelper (H lst)
          Just "string" -> stringHelper (H lst)
          Just "null" -> Right Nul
          _ -> Left "type is not a valid string"

-- | converts an HSON object to
-- | an HSONSchema object representing a number property
numberHelper :: HSON -> Either String HSONSchema
numberHelper (H x) =
  let map = Map.fromList x
   in do
        minVal <- matchNumber "minimum is not a number" "minimum" map
        maxVal <- matchNumber "maximum is not a number" "maximum" map
        exclusiveMin <-
          matchNumber
            "exclusiveMinimum is not a number"
            "exclusiveMinimum"
            map
        exclusiveMax <-
          matchNumber
            "exclusiveMaximum is not a number"
            "exclusiveMaximum"
            map
        multipleOf <-
          matchNumber
            "multipleOf is not a number"
            "multipleOf"
            map
        enum <- case Map.lookup "enum" map of
          Just (Array y) ->
            if isArrayConsistent y
              then Right $ Just $ filterNumberArray y
              else Left "enum has elements of different types"
          Just _ -> Left "enum is not an array"
          Nothing -> Right Nothing
        return $
          Num $
            NP
              { nMinimum = minVal,
                nMaximum = maxVal,
                nExclusiveMinimum = exclusiveMin,
                nExclusiveMaximum = exclusiveMax,
                nMultipleOf = multipleOf,
                numberEnum = enum
              }

-- | converts an HSON object to
-- | an HSONSchema object representing an integer property
intHelper :: HSON -> Either String HSONSchema
intHelper (H x) =
  let map = Map.fromList x
   in do
        minVal <-
          matchInt
            "minimum is not an integer"
            "minimum"
            map
        maxVal <-
          matchInt
            "maximum is not an integer"
            "maximum"
            map
        exclusiveMin <-
          matchInt
            "exclusiveMinimum is not an integer"
            "exclusiveMinimum"
            map
        exclusiveMax <-
          matchInt
            "exclusiveMaximum is not an integer"
            "exclusiveMaximum"
            map
        multipleOf <-
          matchInt
            "multipleOf is not an integer"
            "multipleOf"
            map
        enum <- case Map.lookup "enum" map of
          Just (Array y) ->
            if isArrayConsistent y
              then Right $ Just $ filterIntArray y
              else Left "enum has elements of different types"
          Just _ -> Left "enum is not an array"
          Nothing -> Right Nothing
        return $
          Int $
            IP
              { iMinimum = minVal,
                iMaximum = maxVal,
                iExclusiveMinimum = exclusiveMin,
                iExclusiveMaximum = exclusiveMax,
                iMultipleOf = multipleOf,
                intEnum = enum
              }

-- | converts an HSON object to
-- | an HSONSchema object representing a string property
stringHelper :: HSON -> Either String HSONSchema
stringHelper (H x) =
  let map = Map.fromList x
   in do
        minLength <-
          matchInt
            "minLength is not an integer"
            "minLength"
            map
        maxLength <-
          matchInt
            "maxLength is not an integer"
            "maxLength"
            map
        strPattern <-
          matchString
            "pattern is not a string"
            "pattern"
            map
        enum <- case Map.lookup "enum" map of
          Just (Array y) ->
            if isArrayConsistent y
              then Right $ Just $ filterStringArray y
              else Left "enum has elements of different types"
          Just _ -> Left "enum is not an array"
          Nothing -> Right Nothing
        return $
          Str $
            SP
              { minLength = minLength,
                maxLength = maxLength,
                pattern = strPattern,
                stringEnum = enum
              }

-- | converts an HSON object to
-- | an HSONSchema object representing a boolean property
boolHelper :: HSON -> Either String HSONSchema
boolHelper (H x) =
  let map = Map.fromList x
   in do
        enum <- case Map.lookup "enum" map of
          Just (Array y) ->
            if isArrayConsistent y
              then Right $ Just $ filterBoolArray y
              else Left "enum has elements of different types"
          Just _ -> Left "enum is not an array"
          Nothing -> Right Nothing
        return $ Bool $ BP {boolEnum = boolArrayHelper =<< enum}

-- | converts an HSON object to
-- | an HSONSchema object representing an array property
arrHelper :: HSON -> Either String HSONSchema
arrHelper (H x) =
  let map = Map.fromList x
   in do
        minItems <- matchInt "minItems is not an integer" "minItems" map
        maxItems <- matchInt "maxItems is not an integer" "maxItems" map
        isUnique <- matchBool "isUnique is not a boolean" "isUnique" map
        items <- case Map.lookup "items" map of
          Just (Object hson) -> case schemaParser hson of
            Right y -> Right $ Just y
            Left x -> Left x
          Just _ -> Left "items is not an object"
          Nothing -> Right Nothing
        return $
          Arr $
            AP
              { minItems = minItems,
                maxItems = maxItems,
                isUnique = isJust isUnique && Maybe.fromJust isUnique,
                items = items
              }

-- | converts an HSON object to
-- | an HSONSchema object representing an object property
objHelper :: HSON -> Either String HSONSchema
objHelper (H x) =
  let map = Map.fromList x
   in do
        minProperties <-
          matchInt
            "minProperties is not an integer"
            "minProperties"
            map
        maxProperties <-
          matchInt
            "maxProperties is not an integer"
            "maxProperties"
            map
        required <- case Map.lookup "required" map of
          Just (Array y) ->
            if length y == length (filterStringArray y)
              then Right $ filterStringArray y
              else Left "required contains non-string element(s)"
          Just _ -> Left "required is not an array"
          _ -> Right []
        properties <- case Map.lookup "properties" map of
          Just (Object y) ->
            let parsedProperties = getProperties y
             in getErrorMessages parsedProperties
          Just _ -> Left "properties is not an object"
          Nothing -> Right []
        return $
          Obj $
            OP
              { minProperties = minProperties,
                maxProperties = maxProperties,
                required = required,
                properties = properties
              }

-- | returns a concatenation of all error messages (if any), or returns
-- | a list of all (key, HSONSchema) pairs in the original list
getErrorMessages ::
  [Either String (Key, HSONSchema)] -> Either String [(Key, HSONSchema)]
getErrorMessages = Data.List.foldr combine (Right [])
  where
    combine x acc = case (x, acc) of
      (Right item, Right lst) -> Right (item : lst)
      (Right item, Left err) -> Left err
      (Left curErr, Right lst) -> Left curErr
      (Left curErr, Left err) -> Left $ err ++ "\n" ++ curErr

-- | obtains all the key value pairs obtained from an HSON object
-- | includes errors if any are encountered while parsing
getProperties :: HSON -> [Either String (Key, HSONSchema)]
getProperties (H lst) = Data.List.foldr combHelper [] lst
  where
    combHelper (key, val) acc = case val of
      Object x ->
        case schemaParser x of
          Right y -> Right (key, y) : acc
          Left x -> Left (key ++ ": " ++ x) : acc
      _ -> Left "attributes is not an object" : acc

--------------------------- HSON to HSONSchema ---------------------------------

-- | converts an entire HSON object to it's corresponding HSONSchema object
hsonToHSONSchema :: HSON -> Either String HSONSchema
hsonToHSONSchema = objHelper

------------------------ JSON Schema to HSONSchema -----------------------------

fromJSONSchema :: String -> IO (Maybe HSONSchema)
fromJSONSchema schema = do
  hson <- parseJSON schema
  case hson of
    Left err -> do
      putStrLn $ "\nPARSING JSON FILE FAILED:\n" ++ err ++ "\n"
      return Nothing
    Right hs -> case hsonToHSONSchema hs of
      Left s -> do
        putStrLn $ "\nPARSING JSON SCHEMA FAILED:\n" ++ s ++ "\n"
        return Nothing
      Right hsonSchema -> return $ Just hsonSchema

--------------------------------------------------------------------------------