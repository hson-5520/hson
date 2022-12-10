module ToHSONSchema where

import Data.Map
import Data.Map qualified as Map
import Data.Maybe
import Data.Maybe qualified as Maybe
import HSON (HSON (H), Key, Value (Array, Boolean, Integer, Null, Number, Object, String))
import HSONSchema (ArrProperties (AP), BoolProperties (BP), HSONSchema (Arr, Bool, Int, Nul, Num, Obj, Str), IntProperties (IP), NumProperties (NP), ObjProperties (OP), StrProperties (SP), boolEnum, exclusiveMaximum, exclusiveMinimum, iMaximum, iMinimum, intEnum, isUnique, items, maxItems, maxLength, maxProperties, minItems, minLength, minProperties, multipleOf, nExclusiveMaximum, nExclusiveMinimum, nMaximum, nMinimum, nMultipleOf, numberEnum, pattern, properties, required, stringEnum)
import Network.Socket (accept)
import Parser qualified as P

------------------------- Helpers  --------------------------------

matchInt :: String -> Map Key Value -> (Bool, Maybe Int)
matchInt s map =
  case Map.lookup s map of
    Just (Integer y) -> (True, Just y)
    Just _ -> (False, Nothing)
    Nothing -> (True, Nothing)

matchNumber :: String -> Map Key Value -> (Bool, Maybe Double)
matchNumber s map =
  case Map.lookup s map of
    Just (Number y) -> (True, Just y)
    Just _ -> (False, Nothing)
    Nothing -> (True, Nothing)

matchBool :: String -> Map Key Value -> (Bool, Maybe Bool)
matchBool s map =
  case Map.lookup s map of
    Just (Boolean y) -> (True, Just y)
    Just _ -> (False, Nothing)
    Nothing -> (True, Nothing)

matchString :: String -> Map Key Value -> (Bool, Maybe String)
matchString s map =
  case Map.lookup s map of
    Just (String y) -> (True, Just y)
    Just _ -> (False, Nothing)
    Nothing -> (True, Nothing)

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
numberHelper :: HSON -> Maybe HSONSchema
numberHelper (H x) =
  let map = Map.fromList x
      minVal = matchInt "minimum" map
      maxVal = matchInt "maximum" map
      exclusiveMin = matchInt "exclusiveMinimum" map
      exclusiveMax = matchInt "exclusiveMaximum" map
      multipleOf = matchNumber "multipleOf" map
      enum = case Map.lookup "enum" map of
        Just (Array y) -> if checkArrayLength y then (True, filterNumberArray y) else (False, [])
        Just _ -> (False, [])
        Nothing -> (True, [])
   in if any not [fst minVal, fst maxVal, fst exclusiveMax, fst exclusiveMin, fst multipleOf, fst enum]
        then Nothing
        else
          Just $
            Num $
              NP
                { nMinimum = snd minVal,
                  nMaximum = snd maxVal,
                  nExclusiveMinimum = snd exclusiveMin,
                  nExclusiveMaximum = snd exclusiveMax,
                  nMultipleOf = snd multipleOf,
                  numberEnum = Just $ snd enum
                }

-- | converts a HSON object representing an int property to an HSON Schema int property
intHelper :: HSON -> Maybe HSONSchema
intHelper (H x) =
  let map = Map.fromList x
      minVal = matchInt "minimum" map
      maxVal = matchInt "maximum" map
      exclusiveMin = matchInt "exclusiveMinimum" map
      exclusiveMax = matchInt "exclusiveMaximum" map
      multipleOf = matchInt "multipleOf" map
      enum = case Map.lookup "enum" map of
        Just (Array y) -> if checkArrayLength y then (True, filterIntArray y) else (False, [])
        Just _ -> (False, [])
        Nothing -> (True, [])
   in if any not [fst minVal, fst maxVal, fst exclusiveMax, fst exclusiveMin, fst multipleOf, fst enum]
        then Nothing
        else
          Just $
            Int $
              IP
                { iMinimum = snd minVal,
                  iMaximum = snd maxVal,
                  exclusiveMinimum = snd exclusiveMin,
                  exclusiveMaximum = snd exclusiveMax,
                  multipleOf = snd multipleOf,
                  intEnum = Just $ snd enum
                }

-- | converts a HSON object representing a string property to an HSON Schema str property
stringHelper :: HSON -> Maybe HSONSchema
stringHelper (H x) =
  let map = Map.fromList x
      minLength = matchInt "minLength" map
      maxLength = matchInt "maxLength" map
      strPattern = matchString "pattern" map
      enum = case Map.lookup "enum" map of
        Just (Array y) -> if checkArrayLength y then (True, filterStringArray y) else (False, [])
        Just _ -> (False, [])
        Nothing -> (True, [])
   in if any not [fst minLength, fst maxLength, fst strPattern, fst enum]
        then Nothing
        else
          Just $
            Str $
              SP
                { minLength = snd minLength,
                  maxLength = snd maxLength,
                  pattern = snd strPattern,
                  stringEnum = Just $ snd enum
                }

-- | converts a HSON object representing a booleaan property to an HSON Schema bool property
boolHelper :: HSON -> Maybe HSONSchema
boolHelper (H x) =
  let map = Map.fromList x
      enum = case Map.lookup "enum" map of
        Just (Array y) -> if checkArrayLength y then (True, filterBoolArray y) else (False, [])
        Just _ -> (False, [])
        Nothing -> (True, [])
   in if fst enum
        then Just $ Bool $ BP {boolEnum = boolArrayHelper $ snd enum}
        else Nothing

-- | converts a HSON object representing an array property to an HSON Schema arr property
-- | Arr (AP MaxItems MinItems isUnique Items)
arrHelper :: HSON -> Maybe HSONSchema
arrHelper (H x) =
  let map = Map.fromList x
      minItems = matchInt "minItems" map
      maxItems = matchInt "maxItems" map
      isUnique = matchBool "isUnique" map
      items = case Map.lookup "items" map of
        Just (Object (H x)) -> case objHelper (H x) of
          Just y -> (True, Just y)
          Nothing -> (False, Nothing)
        Just _ -> (False, Nothing)
        Nothing -> (True, Nothing)
   in if any not [fst minItems, fst maxItems, fst isUnique, fst items]
        then Nothing
        else
          Just $
            Arr $
              AP
                { minItems = snd minItems,
                  maxItems = snd maxItems,
                  isUnique = Maybe.fromJust $ snd isUnique,
                  items = snd items
                }

-- | converts a HSON object representing an object property to an HSON Schema obj property
objHelper :: HSON -> Maybe HSONSchema
objHelper (H x) =
  let map = Map.fromList x
      minProperties = matchInt "minProperties" map
      maxProperties = matchInt "maxProperties" map
      required = case Map.lookup "items" map of
        Just (Array y) -> if checkArrayLength y then (True, filterStringArray y) else (False, [])
        Just _ -> (False, [])
        Nothing -> (True, [])
      properties = case Map.lookup "items" map of
        Just (Object (H x)) -> objHelper (H x)
        _ -> Nothing
   in if any not [fst minProperties, fst maxProperties, fst required]
        then Nothing
        else
          Just $
            Obj $
              OP
                { minProperties = snd minProperties,
                  maxProperties = snd maxProperties,
                  required = snd required,
                  properties = []
                }

-- | converts an entire HSON object to it's corresponding HSONSchema object
hsonToHSONSchema :: HSON -> Maybe HSONSchema
hsonToHSONSchema = objHelper
