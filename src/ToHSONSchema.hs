module ToHSONSchema where

import Data.Map
import Data.Map qualified as Map
import Data.Maybe
import HSON (HSON (H), Key, Value (Array, Boolean, Integer, Null, Number, Object, String))
import HSONSchema (HSONSchema (Arr, Bool, Int, Nul, Num, Obj, Str), IntProperties (IP), StrProperties (SP), exclusiveMaximum, exclusiveMinimum, iMaximum, iMinimum, intEnum, maxLength, minLength, multipleOf, pattern, stringEnum)
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

------------------------- HSON to HSON Schema  ---------------------------------

-- | converts a HSON object representing a number property to an HSON Schema num property
-- | Num (IP Minimum ExclusiveMinimum Maximum ExclusiveMaximum)
numberHelper :: HSON -> Maybe HSONSchema
numberHelper (H x) =
  let map = Map.fromList x
      minVal = matchInt "minimum" map
      maxVal = matchInt "maximum" map
      exclusiveMin = matchInt "exclusiveMinimum" map
      exclusiveMax = matchInt "exclusiveMaximum" map
      multipleOf = matchInt "multipleOf" map
      enum = case Map.lookup "enum" map of
        Just (Array y) -> Just y
        _ -> Nothing
   in if any not [fst minVal, fst maxVal, fst exclusiveMax, fst exclusiveMin, fst multipleOf]
        then Nothing
        else
          Just $
            Num $
              IP
                { iMinimum = snd minVal,
                  iMaximum = snd maxVal,
                  exclusiveMinimum = snd exclusiveMin,
                  exclusiveMaximum = snd exclusiveMax,
                  multipleOf = snd multipleOf,
                  intEnum = Nothing
                }

-- | converts a HSON object representing a string property to an HSON Schema str property
-- | Str (SP MinLength MaxLength Pattern)
intHelper :: HSON -> Maybe HSONSchema
intHelper (H x) =
  let map = Map.fromList x
      minVal = matchInt "minimum" map
      maxVal = matchInt "maximum" map
      exclusiveMin = matchInt "exclusiveMinimum" map
      exclusiveMax = matchInt "exclusiveMaximum" map
      multipleOf = matchInt "multipleOf" map
      enum = case Map.lookup "enum" map of
        Just (Array y) -> Just y
        _ -> Nothing
   in if any not [fst minVal, fst maxVal, fst exclusiveMax, fst exclusiveMin, fst multipleOf]
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
                  intEnum = Nothing
                }

-- | converts a HSON object representing a booleaan property to an HSON Schema bool property
-- | Bool (BP)
boolHelper :: HSON -> HSONSchema
boolHelper x = undefined

-- | converts a HSON object representing an array property to an HSON Schema arr property
-- | Arr (AP MaxItems MinItems isUnique Items)
arrHelper :: HSON -> HSONSchema
arrHelper x = undefined

-- | converts a HSON object representing an object property to an HSON Schema obj property
-- | Obj (OP MinProperties MaxProperties Required)
objHelper :: HSON -> HSONSchema
objHelper x = undefined

-- | converts an entire HSON object to it's corresponding HSONSchema object
hsonToHSONSchema :: HSON -> HSONSchema
hsonToHSONSchema hson = objHelper hson
