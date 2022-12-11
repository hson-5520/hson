module HSONValidator where

import Data.Fixed qualified
import Data.Maybe (isNothing)
import Data.Maybe qualified as Maybe
import HSON (HSON, Value (Integer, Number, String))
import HSONSchema (ArrProperties (AP), BoolProperties (BP), HSONSchema (Arr, Bool, Int, Nul, Num, Obj, Str), IntProperties (IP), NumProperties (NP), ObjProperties (OP), StrProperties (SP), boolEnum, iExclusiveMaximum, iExclusiveMinimum, iMaximum, iMinimum, iMultipleOf, intEnum, isUnique, items, maxItems, maxLength, maxProperties, minItems, minLength, minProperties, nExclusiveMaximum, nExclusiveMinimum, nMaximum, nMinimum, nMultipleOf, numberEnum, pattern, properties, required, stringEnum)

------------------------- Helpers  ------------------------------

maybeValidate :: Maybe a -> b -> (b -> a -> Bool) -> Bool
maybeValidate property num f = isNothing property || f num (Maybe.fromJust property)

------------------------- Validating HSON Values  ------------------------------

-- -- definition of the schema type
newtype Schema a = S {validate :: a -> Value -> Bool}

-- | Check if a non-integer number meets its required properties
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
  _ -> False

-- | Check if a non-integer number meets its required properties

-- | Check if an integer meets its required properties
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

-- | Check if a string meets its required properties
validateString :: Schema StrProperties
validateString = S $ \property value -> case value of
  String x ->
    let min = minLength property
        max = maxLength property
        pat = pattern property
        enum = stringEnum property
     in maybeValidate min (length x) (>=)
          && maybeValidate max (length x) (<=)
          -- && maybeValidate exclusiveMin x (\num property -> mod num property == 0)
          && maybeValidate enum x elem
  _ -> False

-- | Check if an array meets its required properties
validateArr :: [Value] -> ArrProperties -> Bool
validateArr = undefined

-- | Check if an object meets its required properties
validateObj :: HSON -> ObjProperties -> Bool
validateObj = undefined

------------------------- Validating HSON  -------------------------------------

validateHSON :: HSON -> HSONSchema -> Bool
validateHSON = undefined
