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
import Text.Regex (matchRegex, matchRegexAll, mkRegex, splitRegex)
import ToJSON

--------------------------------- Helpers  -------------------------------------

-- | takes a Maybe value and returns True if it's nothing or if it is satisfies
-- | the given constraint
maybeValidate ::
  String ->
  Maybe t1 ->
  t2 ->
  (t2 -> t1 -> Bool) ->
  Either String Bool
maybeValidate errMessage property num f = case property of
  Nothing -> Right True
  Just p -> if f num p then Right True else Left errMessage

-- | returns a new list containing all the unique elements of the given list
uniqueElems :: Eq b => [b] -> [b]
uniqueElems = Data.List.map head . group

-- | validating that all the required keys are present in the HSON object
requiredKeysPresent :: [String] -> Map Key Value -> Either String Bool
requiredKeysPresent req map = Data.List.foldr combine (Right True) req
  where
    combine x acc = case acc of
      Left x -> Left x
      Right y ->
        case Map.lookup x map of
          Just z -> Right True
          Nothing -> Left $ x ++ "| doesn't exist"

------------------------- Validating HSON Values  ------------------------------

newtype Schema a = S {validate :: a -> Value -> Either String Bool}

-- | validates all the keys in the HSON object against their schemas
validateAttributes :: [(Key, Value)] -> Map Key HSONSchema -> Either String Bool
validateAttributes dat schema = Data.List.foldr combine (Right True) dat
  where
    combine (key, val) acc =
      do
        case Map.lookup key schema of
          Just y -> case y of
            Num n -> validateAttribute key acc (validate validateNum n val)
            Int i -> validateAttribute key acc (validate validateInt i val)
            Str s -> validateAttribute key acc (validate validateString s val)
            Bool b -> validateAttribute key acc (validate validateBool b val)
            Nul -> validateAttribute key acc (if val == Null then Right True else Left "| is not null but should be")
            Arr a -> validateAttribute key acc (validate validateArr a val)
            Obj o -> validateAttribute (key ++ ".") acc (validate validateObj o val)
          Nothing -> acc
        acc
    validateAttribute key acc res = case acc of
      Left cumulativeErrors -> case res of
        Left currErr -> Left $ cumulativeErrors ++ "\n" ++ key ++ currErr
        Right bool -> Left cumulativeErrors
      Right b -> case res of
        Left currErr -> Left $ key ++ currErr
        Right bool -> return bool

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
     in do
          maybeValidate "| provided number is not >= minimum" min x (>=)
          maybeValidate "| provided number is not <= max" max x (<=)
          maybeValidate "| provided number is not > minimum" exclusiveMin x (>)
          maybeValidate "| provided number is not < max" exclusiveMax x (<)
          maybeValidate "| provided number is not in provided enum" enum x elem
          maybeValidate
            "| provided number is not a multiple of the multipleOf argument"
            multipleOf
            x
            (\num property -> Data.Fixed.mod' num property == 0)
  Integer y ->
    let x = fromIntegral y
        min = nMinimum property
        max = nMaximum property
        exclusiveMin = nExclusiveMinimum property
        exclusiveMax = nExclusiveMaximum property
        multipleOf = nMultipleOf property
        enum = numberEnum property
     in do
          maybeValidate "| provided number is not >= minimum" min x (>=)
          maybeValidate "| provided number is not <= max" max x (<=)
          maybeValidate "| provided number is not > minimum" exclusiveMin x (>)
          maybeValidate "| provided number is not < max" exclusiveMax x (<)
          maybeValidate "| provided number is not in provided enum" enum x elem
          maybeValidate
            "| provided number is not a multiple of the multipleOf argument"
            multipleOf
            x
            (\num property -> Data.Fixed.mod' num property == 0)
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
     in do
          maybeValidate "| provided number is not >= minimum" min x (>=)
          maybeValidate "| provided number is not <= max" max x (<=)
          maybeValidate "| provided number is not > minimum" exclusiveMin x (>)
          maybeValidate "| provided number is not < max" exclusiveMax x (<)
          maybeValidate "| provided number is not in provided enum" enum x elem
          maybeValidate
            "| provided number is not a multiple of the multipleOf argument"
            multipleOf
            x
            (\num property -> mod num property == 0)
  _ -> Left "| provided value is not an int"

-- | create a schema to check if a string meets its required properties
validateString :: Schema StrProperties
validateString = S $ \property value -> case value of
  String x ->
    let min = minLength property
        max = maxLength property
        pat = pattern property
        enum = stringEnum property
     in do
          maybeValidate "| length of string is too small" min (length x) (>=)
          maybeValidate "| length of string is too large" max (length x) (<=)
          maybeValidate "| string is not in provided enum" enum x elem
          case pat of
            Nothing -> return True
            Just p -> case matchRegexAll (mkRegex p) x of
              Nothing -> Left "| regex for string is not satisfied"
              Just (_, matched, _, _) ->
                if matched == x
                  then return True
                  else Left "| regex for string is not satisfied"
          return True
  _ -> Left "| provided value is not a string"

-- | create a schema to check if a bool meets its required properties
validateBool :: Schema BoolProperties
validateBool = S $ \property value -> case value of
  Boolean x ->
    let enum = boolEnum property
     in maybeValidate "| boolean is not the provided enum" enum x (==)
  _ -> Left "| provided value is not a boolean"

-- | create a schema to check if an array meets its required properties
validateArr :: Schema ArrProperties
validateArr = S $ \property value -> case value of
  Array x ->
    let min = minItems property
        max = maxItems property
        uniq = isUnique property
        itemProps = items property
     in do
          maybeValidate "| too few items in array" min (length x) (>=)
          maybeValidate "| too many items in array" max (length x) (<=)
          maybeValidate
            "| at least one item does not meet provided item schema"
            itemProps
            x
            validateItems
          if not uniq || (length (uniqueElems x) == length x)
            then return True
            else Left "| array does not contain unique items"
  _ -> Left "| provided value is not an array"

-- | validating that all items in an array satisfy the input HSONSchema
validateItems :: [Value] -> HSONSchema -> Bool
validateItems val schem = case schem of
  Str s -> all (isRight . validate validateString s) val
  Int i -> all (isRight . validate validateInt i) val
  Num n -> all (isRight . validate validateNum n) val
  Bool b -> all (isRight . validate validateBool b) val
  Arr a -> all (isRight . validate validateArr a) val
  Obj o -> all (isRight . validate validateObj o) val
  Nul -> all (== Null) val

-- | create a schema to check if an array meets its required properties
validateObj :: Schema ObjProperties
validateObj = S $ \property value -> case value of
  Object (H x) ->
    let min = minProperties property
        max = maxProperties property
        req = required property
        props = properties property
     in do
          maybeValidate
            "| too few properties in given object"
            min
            (length x)
            (>=)
          maybeValidate
            "| too many properties in given object"
            max
            (length x)
            (<=)
          requiredKeysPresent req (Map.fromList x)
          validateAttributes x (Map.fromList props)
  _ -> Left "| provided value is not an object"

------------------------- Validating HSON  -------------------------------------

-- | validates that the given HSON obeys the given HSON Schema
validateHSON :: HSON -> HSONSchema -> Either String Bool
validateHSON hson schema = case schema of
  Obj x -> validate validateObj x (Object hson)
  _ -> Left "provided object is not a valid HSON"

--------------------------------------------------------------------------------