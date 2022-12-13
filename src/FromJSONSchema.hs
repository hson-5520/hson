module FromJSONSchema where

import Control.Exception
import Data.Bits (Bits (xor))
import Data.Map
import Data.Map qualified as Map
import Data.Maybe
import Data.Maybe qualified as Maybe
import FromJSON
import GHC.Generics qualified as Maybe
import HSON (HSON (H), Key, Value (Array, Boolean, Integer, Null, Number, Object, String))
import HSONSchema (ArrProperties (AP), BoolProperties (BP), HSONSchema (Arr, Bool, Int, Nul, Num, Obj, Str), IntProperties (IP), NumProperties (NP), ObjProperties (OP), StrProperties (SP), boolEnum, iExclusiveMaximum, iExclusiveMinimum, iMaximum, iMinimum, iMultipleOf, intEnum, isUnique, items, maxItems, maxLength, maxProperties, minItems, minLength, minProperties, nExclusiveMaximum, nExclusiveMinimum, nMaximum, nMinimum, nMultipleOf, numberEnum, pattern, properties, required, stringEnum)
import Network.Socket (accept)
import Parser (parse)
import Parser qualified as P
import Test.HUnit
import ToJSON

------------------------- Helpers  --------------------------------

matchInt :: String -> Map Key Value -> Either String (Maybe Int)
matchInt s map =
  case Map.lookup s map of
    Just (Integer y) -> Right $ Just y
    Just _ -> Left ""
    Nothing -> Right Nothing

matchNumber :: String -> Map Key Value -> Either String (Maybe Double)
matchNumber s map =
  case Map.lookup s map of
    Just (Number y) -> Right $ Just y
    Just (Integer x) -> Right $ Just (fromIntegral x)
    Just _ -> Left ""
    Nothing -> Right Nothing

matchBool :: String -> Map Key Value -> Either String (Maybe Bool)
matchBool s map =
  case Map.lookup s map of
    Just (Boolean y) -> Right $ Just y
    Just _ -> Left ""
    Nothing -> Right Nothing

matchString :: String -> Map Key Value -> Either String (Maybe String)
matchString s map =
  case Map.lookup s map of
    Just (String y) -> Right (Just y)
    Just _ -> Left ""
    Nothing -> Right Nothing

filterIntArray :: [Value] -> [Int]
filterIntArray = Prelude.foldr combHelper []
  where
    combHelper x acc = case x of
      Integer y -> y : acc
      _ -> acc

filterNumberArray :: [Value] -> [Double]
filterNumberArray = Prelude.foldr combHelper []
  where
    combHelper x acc = case x of
      Number y -> y : acc
      _ -> acc

filterBoolArray :: [Value] -> [Bool]
filterBoolArray = Prelude.foldr combHelper []
  where
    combHelper x acc = case x of
      Boolean y -> y : acc
      _ -> acc

filterStringArray :: [Value] -> [String]
filterStringArray = Prelude.foldr combHelper []
  where
    combHelper x acc = case x of
      String y -> y : acc
      _ -> acc

--- >>> filterStringArray [String "latitude", String "longitude"]

checkArrayLength :: [Value] -> Bool
checkArrayLength x =
  length x == length (filterStringArray x)
    || length x == length (filterIntArray x)
    || length x == length (filterNumberArray x)
    || length x == length (filterBoolArray x)

boolArrayHelper :: [Bool] -> Maybe Bool
boolArrayHelper x
  | Prelude.null x = Nothing
  | all (== True) x = Just True
  | all (== False) x = Just False
  | otherwise = Nothing

------------------------- HSON to HSON Schema  ---------------------------------

-- | converts a HSON object representing a number property to an HSON Schema num property
numberHelper :: HSON -> Either String HSONSchema
numberHelper (H x) =
  let map = Map.fromList x
   in do
        minVal <- matchNumber "minimum" map
        maxVal <- matchNumber "maximum" map
        exclusiveMin <- matchNumber "exclusiveMinimum" map
        exclusiveMax <- matchNumber "exclusiveMaximum" map
        multipleOf <- matchNumber "multipleOf" map
        enum <- case Map.lookup "enum" map of
          Just (Array y) -> if checkArrayLength y then Right $ Just $ filterNumberArray y else Left ""
          Just _ -> Left ""
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

intHelper :: HSON -> Either String HSONSchema
intHelper (H x) =
  let map = Map.fromList x
   in do
        minVal <- matchInt "minimum" map
        maxVal <- matchInt "maximum" map
        exclusiveMin <- matchInt "exclusiveMinimum" map
        exclusiveMax <- matchInt "exclusiveMaximum" map
        multipleOf <- matchInt "multipleOf" map
        enum <- case Map.lookup "enum" map of
          Just (Array y) -> if checkArrayLength y then Right $ Just $ filterIntArray y else Left ""
          Just _ -> Left ""
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

-- | converts a HSON object representing a string property to an HSON Schema str property
stringHelper :: HSON -> Either String HSONSchema
stringHelper (H x) =
  let map = Map.fromList x
   in do
        minLength <- matchInt "minLength" map
        maxLength <- matchInt "maxLength" map
        strPattern <- matchString "pattern" map
        enum <- case Map.lookup "enum" map of
          Just (Array y) -> if checkArrayLength y then Right $ Just $ filterStringArray y else Left ""
          Just _ -> Left ""
          Nothing -> Right Nothing
        return $
          Str $
            SP
              { minLength = minLength,
                maxLength = maxLength,
                pattern = strPattern,
                stringEnum = enum
              }

-- | converts a HSON object representing a booleaan property to an HSON Schema bool property
boolHelper :: HSON -> Either String HSONSchema
boolHelper (H x) =
  let map = Map.fromList x
   in do
        enum <- case Map.lookup "enum" map of
          Just (Array y) -> if checkArrayLength y then Right $ Just $ filterBoolArray y else Left ""
          Just _ -> Left ""
          Nothing -> Right Nothing
        return $ Bool $ BP {boolEnum = boolArrayHelper =<< enum}

-- | converts a HSON object representing an array property to an HSON Schema arr property
arrHelper :: HSON -> Either String HSONSchema
arrHelper (H x) =
  let map = Map.fromList x
   in do
        minItems <- matchInt "minItems" map
        maxItems <- matchInt "maxItems" map
        isUnique <- matchBool "isUnique" map
        items <- case Map.lookup "items" map of
          Just (Object hson) -> case schemaParser hson of
            Right y -> Right $ Just y
            Left x -> Left x
          Just _ -> Left ""
          Nothing -> Right Nothing
        return $
          Arr $
            AP
              { minItems = minItems,
                maxItems = maxItems,
                isUnique = isJust isUnique && Maybe.fromJust isUnique,
                items = items
              }

-- | converts a HSON object representing an object property to an HSON Schema obj property
objHelper :: HSON -> Either String HSONSchema
objHelper (H x) =
  let map = Map.fromList x
   in do
        minProperties <- matchInt "minProperties" map
        maxProperties <- matchInt "maxProperties" map
        required <- case Map.lookup "required" map of
          Just (Array y) ->
            if length y == length (filterStringArray y)
              then Right $ filterStringArray y
              else Left ""
          Just _ -> Left ""
          _ -> Right []
        properties <- case Map.lookup "properties" map of
          Just (Object y) ->
            let parsedProperties = getProperties y
             in if any isNothing parsedProperties
                  then Left ""
                  else Right $ fmap Maybe.fromJust parsedProperties
          Just _ -> Left ""
          Nothing -> Right []
        return $
          Obj $
            OP
              { minProperties = minProperties,
                maxProperties = maxProperties,
                required = required,
                properties = properties
              }

schemaParser :: HSON -> Either String HSONSchema
schemaParser (H lst) =
  let map = Map.fromList lst
   in do
        str <- matchString "type" map
        case str of
          Nothing -> Left ""
          Just "number" -> numberHelper (H lst)
          Just "integer" -> intHelper (H lst)
          Just "boolean" -> boolHelper (H lst)
          Just "array" -> arrHelper (H lst)
          Just "object" -> objHelper (H lst)
          Just "string" -> stringHelper (H lst)
          Just "null" -> Right Nul
          _ -> Left ""

getProperties :: HSON -> [Maybe (Key, HSONSchema)]
getProperties (H lst) = Prelude.foldr combHelper [] lst
  where
    combHelper (key, val) acc = case val of
      Object x ->
        case schemaParser x of
          Right y -> Just (key, y) : acc
          Left x -> Nothing : acc
      _ -> Nothing : acc

-- | converts an entire HSON object to it's corresponding HSONSchema object
hsonToHSONSchema :: HSON -> Either String HSONSchema
hsonToHSONSchema = objHelper
