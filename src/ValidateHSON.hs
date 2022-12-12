module ValidateHSON where

import Data.Either (isRight)
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
maybeValidate2 :: String -> Maybe a -> b -> (b -> a -> Bool) -> Either String Bool
maybeValidate2 errMessage property num f =
  if isNothing property || f num (Maybe.fromJust property)
    then Right True
    else Left errMessage

maybeValidate :: Maybe a -> b -> (b -> a -> Bool) -> Bool
maybeValidate property num f = isNothing property || f num (Maybe.fromJust property)

-- | returns a new list containing all the unique elements of the given list
uniqueElems :: Eq b => [b] -> [b]
uniqueElems = Data.List.map head . group

------------------------- Validating HSON Values  ------------------------------

newtype Schema a = S {validate :: a -> Value -> Bool}

newtype Schema2 a = S2 {validate2 :: a -> Value -> Either String Bool}

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
          && maybeValidate multipleOf x (\num property -> Data.Fixed.mod' num property == 0)
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
          && maybeValidate multipleOf x (\num property -> Data.Fixed.mod' num property == 0)
          && maybeValidate enum x elem
  _ -> False

validateNum2 :: Schema2 NumProperties
validateNum2 = S2 $ \property value -> case value of
  Number x ->
    let min = nMinimum property
        max = nMaximum property
        exclusiveMin = nExclusiveMinimum property
        exclusiveMax = nExclusiveMaximum property
        multipleOf = nMultipleOf property
        enum = numberEnum property
     in do
          maybeValidate2 "| provided number is not >= minimum" min x (>=)
          maybeValidate2 "| provided number is not <= max" max x (<=)
          maybeValidate2 "| provided number is not > minimum" exclusiveMin x (>)
          maybeValidate2 "| provided number is not < max" exclusiveMax x (<)
          maybeValidate2 "| provided number is not in provided enum" enum x elem
          maybeValidate2 "| provided number is not a multiple of the multipleOf argument" multipleOf x (\num property -> Data.Fixed.mod' num property == 0)
  Integer y ->
    let x = fromIntegral y
        min = nMinimum property
        max = nMaximum property
        exclusiveMin = nExclusiveMinimum property
        exclusiveMax = nExclusiveMaximum property
        multipleOf = nMultipleOf property
        enum = numberEnum property
     in do
          maybeValidate2 "| provided number is not >= minimum" min x (>=)
          maybeValidate2 "| provided number is not <= max" max x (<=)
          maybeValidate2 "| provided number is not > minimum" exclusiveMin x (>)
          maybeValidate2 "| provided number is not < max" exclusiveMax x (<)
          maybeValidate2 "| provided number is not in provided enum" enum x elem
          maybeValidate2 "| provided number is not a multiple of the multipleOf argument" multipleOf x (\num property -> Data.Fixed.mod' num property == 0)
  _ -> Left "| provided value is not a number or int"

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

validateInt2 :: Schema2 IntProperties
validateInt2 = S2 $ \property value -> case value of
  Integer x ->
    let min = iMinimum property
        max = iMaximum property
        exclusiveMin = iExclusiveMinimum property
        exclusiveMax = iExclusiveMaximum property
        multipleOf = iMultipleOf property
        enum = intEnum property
     in do
          maybeValidate2 "| provided number is not >= minimum" min x (>=)
          maybeValidate2 "| provided number is not <= max" max x (<=)
          maybeValidate2 "| provided number is not > minimum" exclusiveMin x (>)
          maybeValidate2 "| provided number is not < max" exclusiveMax x (<)
          maybeValidate2 "| provided number is not in provided enum" enum x elem
          maybeValidate2 "| provided number is not a multiple of the multipleOf argument" multipleOf x (\num property -> mod num property == 0)
  _ -> Left "| provided value is not an int"

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

validateString2 :: Schema2 StrProperties
validateString2 = S2 $ \property value -> case value of
  String x ->
    let min = minLength property
        max = maxLength property
        pat = pattern property
        enum = stringEnum property
     in do
          maybeValidate2 "| length of string is too small" min (length x) (>=)
          maybeValidate2 "| length of string is too large" max (length x) (<=)
          maybeValidate2 "| string is not in provided enum" enum x elem
          if isNothing pat
            then return True
            else
              ( case matchRegexAll (mkRegex (Maybe.fromJust pat)) x of
                  Nothing -> Left "| regex for string is not satisfied"
                  Just (_, matched, _, _) -> if matched == x then return True else Left "|regex for string is not satisfied"
              )
          return True
  _ -> Left "| provided value is not a string"

-- | create a schema to check if a bool meets its required properties
validateBool :: Schema BoolProperties
validateBool = S $ \property value -> case value of
  Boolean x ->
    let enum = boolEnum property
     in maybeValidate enum x (==)
  _ -> False

validateBool2 :: Schema2 BoolProperties
validateBool2 = S2 $ \property value -> case value of
  Boolean x ->
    let enum = boolEnum property
     in maybeValidate2 "| boolean is not the provided enum" enum x (==)
  _ -> Left "| provided value is not a boolean"

-- | validating that all items in an array satisfy the input HSONSchema
validateItems :: [Value] -> HSONSchema -> Bool
validateItems val schem = case schem of
  Str s -> all (isRight . validate2 validateString2 s) val
  Int i -> all (isRight . validate2 validateInt2 i) val
  Num n -> all (isRight . validate2 validateNum2 n) val
  Bool b -> all (isRight . validate2 validateBool2 b) val
  Arr a -> all (isRight . validate2 validateArr2 a) val
  Obj o -> all (isRight . validate2 validateObj2 o) val
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

-- | create a schema to check if an array meets its required properties
validateArr2 :: Schema2 ArrProperties
validateArr2 = S2 $ \property value -> case value of
  Array x ->
    let min = minItems property
        max = maxItems property
        uniq = isUnique property
        itemProps = items property
     in do
          maybeValidate2 "| too few items in array" min (length x) (>=)
          maybeValidate2 "| too many items in array" max (length x) (<=)
          maybeValidate2 "| at least one item does not meet provided item schema" itemProps x validateItems
          if not uniq || (length (uniqueElems x) == length x)
            then return True
            else Left "| array does not contain unique items"
  _ -> Left "| provided value is not an array"

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

-- | validating that all the required keys are present in the HSON object
requiredKeysPresent2 :: [String] -> Map Key Value -> Either String Bool
requiredKeysPresent2 req map = Data.List.foldr combine (Right True) req
  where
    combine x acc = case acc of
      Left x -> Left x
      Right y ->
        case Map.lookup x map of
          Just z -> Right True
          Nothing -> Left $ x ++ "| doesn't exist"

-- | validating all the keys in the HSON object against their properties
matchProperties2 :: [(Key, Value)] -> Map Key HSONSchema -> Either String Bool
matchProperties2 dat schema = Data.List.foldr combine (Right True) dat
  where
    combine (key, val) acc = case acc of
      Left x -> Left x
      Right y -> do
        case Map.lookup key schema of
          Just y -> case y of
            Str x ->
              case validate2 validateString2 x val of
                Left err -> Left $ key ++ err
                Right bool -> return bool
            Int i -> case validate2 validateInt2 i val of
              Left err -> Left $ key ++ err
              Right bool -> return bool
            Num n -> case validate2 validateNum2 n val of
              Left err -> Left $ key ++ err
              Right bool -> return bool
            Bool b -> case validate2 validateBool2 b val of
              Left err -> Left $ key ++ err
              Right bool -> return bool
            Arr a -> case validate2 validateArr2 a val of
              Left err -> Left $ key ++ err
              Right bool -> return bool
            Obj o -> case validate2 validateObj2 o val of
              Left err -> Left $ key ++ "." ++ err
              Right bool -> return bool
            Nul -> if val == Null then return True else Left $ key ++ "| is not null but should be"
          Nothing -> return True
        return True

-- | create a schema to check if an array meets its required properties
validateObj2 :: Schema2 ObjProperties
validateObj2 = S2 $ \property value -> case value of
  Object (H x) ->
    let min = minProperties property
        max = maxProperties property
        req = required property
        props = properties property
     in do
          maybeValidate2 "| too few properties in given object" min (length x) (>=)
          maybeValidate2 "| too many properties in given object" max (length x) (<=)
          requiredKeysPresent2 req (Map.fromList x)
          matchProperties2 x (Map.fromList props)
  _ -> Left "| provided value is not an object"

------------------------- Validating HSON  -------------------------------------

-- | validates that the given HSON obeys the given HSON Schema
validateHSON :: HSON -> HSONSchema -> Bool
validateHSON hson schema = case schema of
  Obj x -> validate validateObj x (Object hson)
  _ -> False

validateHSON2 :: HSON -> HSONSchema -> Either String Bool
validateHSON2 hson schema = case schema of
  Obj x -> validate2 validateObj2 x (Object hson)
  _ -> Left "provided object is not a valid HSON"
