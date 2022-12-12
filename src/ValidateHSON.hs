module ValidateHSON where

import Data.Fixed qualified
import Data.List
import Data.Map
import Data.Map qualified as Map
import Data.Maybe (isNothing)
import Data.Maybe qualified as Maybe
import FromJSON (itemP, parseJSON)
import FromJSONSchema
import HSON (HSON (H), Key, Value (Array, Boolean, Integer, Null, Number, Object, String))
import HSONSchema (ArrProperties (AP), BoolProperties (BP), HSONSchema (Arr, Bool, Int, Nul, Num, Obj, Str), IntProperties (IP), NumProperties (NP), ObjProperties (OP), StrProperties (SP), boolEnum, iExclusiveMaximum, iExclusiveMinimum, iMaximum, iMinimum, iMultipleOf, intEnum, isUnique, items, maxItems, maxLength, maxProperties, minItems, minLength, minProperties, nExclusiveMaximum, nExclusiveMinimum, nMaximum, nMinimum, nMultipleOf, numberEnum, pattern, properties, required, stringEnum)
import Test.HUnit
import Text.Regex (matchRegex, matchRegexAll, mkRegex, splitRegex)
import ToJSON

--------------------------------- Helpers  -------------------------------------

-- | takes a Maybe value and returns True if it's nothing or if it is satisfies
-- | the given constraint
maybeValidate :: Maybe a -> b -> (b -> a -> Bool) -> Bool
maybeValidate property num f = isNothing property || f num (Maybe.fromJust property)

-- | returns a new list containing all the unique elements of the given list
uniqueElems :: Eq b => [b] -> [b]
uniqueElems = Data.List.map head . group

------------------------- Validating HSON Values  ------------------------------

newtype Schema a = S {validate :: a -> Value -> Bool}

-- | create a schema to check if a number meets its required properties
validateNum :: Schema NumProperties
validateNum = S $ \property value -> case value of
  Number x ->
    let min = nMinimum property
        max = nMaximum property
        exclusiveMin = nExclusiveMinimum property
        exclusiveMax = nExclusiveMaximum property
        multipleOf = nMultipleOf property
        enum = numberEnum property
     in maybeValidate min x (>=)
          && maybeValidate max x (<=)
          && maybeValidate exclusiveMin x (>)
          && maybeValidate exclusiveMax x (<)
          && maybeValidate exclusiveMin x (\num property -> Data.Fixed.mod' num property == 0)
          && maybeValidate enum x elem
  Integer y ->
    let x = fromIntegral y
        min = nMinimum property
        max = nMaximum property
        exclusiveMin = nExclusiveMinimum property
        exclusiveMax = nExclusiveMaximum property
        multipleOf = nMultipleOf property
        enum = numberEnum property
     in maybeValidate min x (>=)
          && maybeValidate max x (<=)
          && maybeValidate exclusiveMin x (>)
          && maybeValidate exclusiveMax x (<)
          && maybeValidate exclusiveMin x (\num property -> Data.Fixed.mod' num property == 0)
          && maybeValidate enum x elem
  _ -> False

-- | create a schema to check if an integer meets its required properties
validateInt :: Schema IntProperties
validateInt = S $ \property value -> case value of
  Integer x ->
    let min = iMinimum property
        max = iMaximum property
        exclusiveMin = iExclusiveMinimum property
        exclusiveMax = iExclusiveMaximum property
        multipleOf = iMultipleOf property
        enum = intEnum property
     in maybeValidate min x (>=)
          && maybeValidate max x (<=)
          && maybeValidate exclusiveMin x (>)
          && maybeValidate exclusiveMax x (<)
          && maybeValidate exclusiveMin x (\num property -> mod num property == 0)
          && maybeValidate enum x elem
  _ -> False

-- | create a schema to check if a string meets its required properties
validateString :: Schema StrProperties
validateString = S $ \property value -> case value of
  String x ->
    let min = minLength property
        max = maxLength property
        pat = pattern property
        enum = stringEnum property
     in maybeValidate min (length x) (>=)
          && maybeValidate max (length x) (<=)
          && maybeValidate enum x elem
          && ( isNothing pat || case matchRegexAll (mkRegex (Maybe.fromJust pat)) x of
                 Nothing -> False
                 Just (_, matched, _, _) -> matched == x
             )
  _ -> False

-- | create a schema to check if a bool meets its required properties
validateBool :: Schema BoolProperties
validateBool = S $ \property value -> case value of
  Boolean x ->
    let enum = boolEnum property
     in maybeValidate enum x (==)
  _ -> False

-- | validating that all items in an array satisfy the input HSONSchema
validateItems :: [Value] -> HSONSchema -> Bool
validateItems val schem = case schem of
  Str x -> all (validate validateString x) val
  Int i -> all (validate validateInt i) val
  Num n -> all (validate validateNum n) val
  Bool b -> all (validate validateBool b) val
  Arr a -> all (validate validateArr a) val
  Obj o -> all (validate validateObj o) val
  Nul -> all (== Null) val

-- | create a schema to check if an array meets its required properties
validateArr :: Schema ArrProperties
validateArr = S $ \property value -> case value of
  Array x ->
    let min = minItems property
        max = maxItems property
        uniq = isUnique property
        itemProps = items property
     in maybeValidate min (length x) (>=)
          && maybeValidate max (length x) (<=)
          && (not uniq || length (uniqueElems x) == length x)
          && maybeValidate itemProps x validateItems
  _ -> False

-- | validating that all the required keys are present in the HSON object
requiredKeysPresent :: [String] -> Map Key Value -> Bool
requiredKeysPresent req map = Data.List.foldr combine True req
  where
    combine x acc =
      acc
        && ( case Map.lookup x map of
               Just y -> True
               Nothing -> False
           )

-- | validating all the keys in the HSON object against their properties
matchProperties :: [(Key, Value)] -> Map Key HSONSchema -> Bool
matchProperties dat schema = Data.List.foldr combine True dat
  where
    combine (key, val) acc =
      acc
        && ( case Map.lookup key schema of
               Just y -> case y of
                 Str x -> validate validateString x val
                 Int i -> validate validateInt i val
                 Num n -> validate validateNum n val
                 Bool b -> validate validateBool b val
                 Arr a -> validate validateArr a val
                 Obj o -> validate validateObj o val
                 Nul -> val == Null
               Nothing -> True
           )

-- | create a schema to check if an object meets its required properties
validateObj :: Schema ObjProperties
validateObj = S $ \property value -> case value of
  Object (H x) ->
    let min = minProperties property
        max = maxProperties property
        req = required property
        props = properties property
     in maybeValidate min (length x) (>=)
          && maybeValidate max (length x) (<=)
          && requiredKeysPresent req (Map.fromList x)
          && matchProperties x (Map.fromList props)
  _ -> False

------------------------- Validating HSON  -------------------------------------

-- | validates that the given HSON obeys the given HSON Schema
validateHSON :: HSON -> HSONSchema -> Bool
validateHSON hson schema = case schema of
  Obj x -> validate validateObj x (Object hson)
  _ -> False
