module ValidateHSONTest where

import Control.Applicative
import Data.Either
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import FromJSON
import FromJSONSchema (hsonToHSONSchema)
import HSON (HSON (..), Key, Value (Array, Boolean, Integer, Null, Number, Object, String), hsonArray, hsonDog, hsonEmpty, hsonSchool, hsonSingle)
import HSONSchema (ArrProperties (..), BoolProperties (..), HSONSchema (..), IntProperties (..), NumProperties (..), ObjProperties (..), StrProperties (..), address, card, coordinate)
import Parser qualified as P
import Test.HUnit (Counts, Test (TestList), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck
import ToJSON
import ValidateHSON

------------------------- Helpers -----------------------------

test_maybeValidate :: Test
test_maybeValidate =
  "test maybeValidate"
    ~: TestList
      [ maybeValidate Nothing 1 (\x y -> False) ~?= True,
        maybeValidate (Just 5) 10 (>=) ~?= True,
        maybeValidate (Just [1, 2, 3]) 3 elem ~?= True
      ]

--- >>> runTestTT test_maybeValidate

test_uniqueElems :: Test
test_uniqueElems =
  "test unique elements in a list"
    ~: TestList
      [ uniqueElems ([] :: [Int]) ~?= ([] :: [Int]),
        uniqueElems [1] ~?= [1],
        uniqueElems [1, 2, 3] ~?= [1, 2, 3],
        uniqueElems [1, 1] ~?= [1],
        uniqueElems [1, 1, 2, 2, 3, 3] ~?= [1, 2, 3]
      ]

--- >>> runTestTT test_uniqueElems
-- Counts {cases = 5, tried = 5, errors = 0, failures = 0}

------------------------- HSON Schema Property Validating Tests -----------------------------

testNP :: NumProperties
testNP =
  NP
    { nMinimum = Nothing,
      nMaximum = Just 5.0,
      nExclusiveMaximum = Nothing,
      nExclusiveMinimum = Just (-1.0),
      nMultipleOf = Just 1.0,
      numberEnum = Just [-1.0, 1.0, 2.5, 4.0, 5.0, 6.0]
    }

test_validateNum :: Test
test_validateNum =
  "test validate number"
    ~: TestList
      [ validate validateNum testNP (Number 4.0) ~?= True,
        validate validateNum testNP (Number 5.0) ~?= True,
        validate validateNum testNP (Number 6.0) ~?= False,
        validate validateNum testNP (Number 2.5) ~?= False,
        validate validateNum testNP (Number 3.0) ~?= False,
        validate validateNum testNP (Number (-1.0)) ~?= False,
        validate validateNum testNP Null ~?= False
      ]

test_validateNum2 :: Test
test_validateNum2 =
  "test validate number"
    ~: TestList
      [ validate2 validateNum2 testNP (Number 4.0) ~?= Right True,
        validate2 validateNum2 testNP (Number 5.0) ~?= Right True,
        validate2 validateNum2 testNP (Number 6.0) ~?= Left "| provided number is not <= max",
        validate2 validateNum2 testNP (Number 2.5) ~?= Left "| provided number is not a multiple of the multipleOf argument",
        validate2 validateNum2 testNP (Number 3.0) ~?= Left "| provided number is not in provided enum",
        validate2 validateNum2 testNP (Number (-1.0)) ~?= Left "| provided number is not > minimum",
        validate2 validateNum2 testNP Null ~?= Left "| provided value is not a number or int"
      ]

--- >>> runTestTT test_validateNum
-- Counts {cases = 7, tried = 7, errors = 0, failures = 0}

--- >>> runTestTT test_validateNum2
-- Counts {cases = 7, tried = 7, errors = 0, failures = 0}

testIP :: IntProperties
testIP =
  IP
    { iMinimum = Nothing,
      iMaximum = Just 5,
      iExclusiveMaximum = Nothing,
      iExclusiveMinimum = Just (-1),
      iMultipleOf = Just 1,
      intEnum = Just [-1, 1, 2, 4, 5, 6]
    }

test_validateInt :: Test
test_validateInt =
  "test validate integer"
    ~: TestList
      [ validate validateInt testIP (Integer 4) ~?= True,
        validate validateInt testIP (Integer 5) ~?= True,
        validate validateInt testIP (Integer 6) ~?= False,
        validate validateInt testIP (Integer 2) ~?= True,
        validate validateInt testIP (Integer 3) ~?= False,
        validate validateInt testIP (Integer (-1)) ~?= False,
        validate validateInt testIP Null ~?= False
      ]

test_validateInt2 :: Test
test_validateInt2 =
  "test validate integer"
    ~: TestList
      [ validate2 validateInt2 testIP (Integer 4) ~?= Right True,
        validate2 validateInt2 testIP (Integer 5) ~?= Right True,
        validate2 validateInt2 testIP (Integer 6) ~?= Left "| provided number is not <= max",
        validate2 validateInt2 testIP (Integer 2) ~?= Right True,
        validate2 validateInt2 testIP (Integer 3) ~?= Left "| provided number is not in provided enum",
        validate2 validateInt2 testIP (Integer (-1)) ~?= Left "| provided number is not > minimum",
        validate2 validateInt2 testIP Null ~?= Left "| provided value is not an int"
      ]

--- >>> runTestTT test_validateInt
-- Counts {cases = 7, tried = 7, errors = 0, failures = 0}

--- >>> runTestTT test_validateInt2
-- Counts {cases = 7, tried = 7, errors = 0, failures = 0}

testSP :: StrProperties
testSP =
  SP
    { minLength = Just 5,
      maxLength = Just 10,
      pattern = Just "a*b",
      stringEnum = Just ["a", "aaaaaab", "aaaaaaa"]
    }

test_validateString :: Test
test_validateString =
  "test validate string"
    ~: TestList
      [ validate validateString testSP (String "aaaaaab") ~?= True,
        validate validateString testSP (String "aaaaaaa") ~?= False,
        validate validateString testSP (String "hello") ~?= False,
        validate validateString testSP (String "aaab") ~?= False,
        validate validateString testSP (String "this is too long") ~?= False,
        validate validateString testSP Null ~?= False
      ]

-- >>> runTestTT test_validateString

test_validateString2 :: Test
test_validateString2 =
  "test validate string"
    ~: TestList
      [ validate2 validateString2 testSP (String "aaaaaab") ~?= Right True,
        validate2 validateString2 testSP (String "aaaaaaa") ~?= Left "| regex for string is not satisfied",
        validate2 validateString2 testSP (String "hello") ~?= Left "| string is not in provided enum",
        validate2 validateString2 testSP (String "aaab") ~?= Left "| length of string is too small",
        validate2 validateString2 testSP (String "this is too long") ~?= Left "| length of string is too large",
        validate2 validateString2 testSP Null ~?= Left "| provided value is not a string"
      ]

-- >>> runTestTT test_validateString2
-- Counts {cases = 6, tried = 6, errors = 0, failures = 0}

testBP :: BoolProperties
testBP = BP {boolEnum = Just True}

test_validateBool :: Test
test_validateBool =
  "test validate bool"
    ~: TestList
      [ validate validateBool testBP (Boolean True) ~?= True,
        validate validateBool testBP (Boolean False) ~?= False,
        validate validateBool testBP Null ~?= False
      ]

test_validateBool2 :: Test
test_validateBool2 =
  "test validate bool"
    ~: TestList
      [ validate2 validateBool2 testBP (Boolean True) ~?= Right True,
        validate2 validateBool2 testBP (Boolean False) ~?= Left "| boolean is not the provided enum",
        validate2 validateBool2 testBP Null ~?= Left "| provided value is not a boolean"
      ]

--- >>> runTestTT test_validateBool
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}

--- >>> runTestTT test_validateBool2
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}

testAP :: ArrProperties
testAP =
  AP
    { minItems = Just 2,
      maxItems = Just 5,
      isUnique = True,
      items = Just (Int testIP)
    }

test_validateArr :: Test
test_validateArr =
  "test validate array"
    ~: TestList
      [ validate validateArr testAP (Array [Integer 2, Integer 4]) ~?= True,
        validate validateArr testAP (Array [Integer 2, Integer 4, Integer 5]) ~?= True,
        validate validateArr testAP (Array [Integer 2]) ~?= False,
        validate validateArr testAP (Array [Integer 2, Integer 2]) ~?= False,
        validate validateArr testAP (Array []) ~?= False,
        validate validateArr testAP (Array [Integer 2, Integer 3]) ~?= False,
        validate validateArr testAP Null ~?= False
      ]

test_validateArr2 :: Test
test_validateArr2 =
  "test validate array"
    ~: TestList
      [ validate2 validateArr2 testAP (Array [Integer 2, Integer 4]) ~?= Right True,
        validate2 validateArr2 testAP (Array [Integer 2, Integer 4, Integer 5]) ~?= Right True,
        validate2 validateArr2 testAP (Array [Integer 2]) ~?= Left "| too few items in array",
        validate2 validateArr2 testAP (Array [Integer 2, Integer 2]) ~?= Left "| array does not contain unique items",
        validate2 validateArr2 testAP (Array []) ~?= Left "| too few items in array",
        validate2 validateArr2 testAP (Array [Integer 2, Integer 3]) ~?= Left "| at least one item does not meet provided item schema",
        validate2 validateArr2 testAP Null ~?= Left "| provided value is not an array"
      ]

--- >>> runTestTT test_validateArr
-- Counts {cases = 7, tried = 7, errors = 0, failures = 0}

--- >>> runTestTT test_validateArr2

testOP :: ObjProperties
testOP =
  OP
    { minProperties = Just 2,
      maxProperties = Just 5,
      required = ["bool"],
      properties =
        [ ("int", Int testIP),
          ("num", Num testNP),
          ("str", Str testSP),
          ("bool", Bool testBP),
          ("arr", Arr testAP),
          ("null", Nul),
          ("obj", Obj testOP)
        ]
    }

test_validateObj :: Test
test_validateObj =
  "test validate object"
    ~: TestList
      [ validate validateObj testOP (Object $ H [("bool", Boolean True)]) ~?= False,
        validate validateObj testOP (Object $ H [("bool", Boolean True), ("num", Number 4.0)]) ~?= True,
        validate validateObj testOP (Object $ H [("bool", Boolean False), ("num", Number 4.0)]) ~?= False,
        validate validateObj testOP (Object $ H [("bool", Boolean True), ("Bill", Null), ("Jim", Null)]) ~?= True,
        validate validateObj testOP (Object $ H [("bool", Boolean True), ("Bill", Number 4.0), ("Jim", Null), ("Mike", Null), ("George", Null), ("Chris", Null)]) ~?= False,
        validate validateObj testOP (Object $ H [("bool", Boolean True), ("null", Null)]) ~?= True,
        validate validateObj testOP (Object $ H [("int", Integer 2), ("null", Null)]) ~?= False,
        validate validateObj testOP (Object $ H [("int", Integer 2), ("null", Number 4.0)]) ~?= False,
        validate validateObj testOP (Object $ H [("bool", Boolean True), ("int", Integer 2), ("null", Null), ("num", Number 4.0), ("str", String "aaaaaab")]) ~?= True,
        validate validateObj testOP (Object $ H [("bool", Boolean True), ("arr", Array [Integer 2, Integer 4])]) ~?= True,
        validate validateObj testOP (Object $ H [("bool", Boolean True), ("obj", Object $ H [("bool", Boolean True), ("num", Number 4.0)])]) ~?= True,
        validate validateObj testOP (Object $ H [("bool", Boolean True), ("obj", Object $ H [("bool", Boolean False), ("num", Number 4.0)])]) ~?= False,
        validate validateObj testOP Null ~?= False
      ]

test_validateObj2 :: Test
test_validateObj2 =
  "test validate object"
    ~: TestList
      [ validate2 validateObj2 testOP (Object $ H [("bool", Boolean True)]) ~?= Left "| too few properties in given object",
        validate2 validateObj2 testOP (Object $ H [("bool", Boolean True), ("num", Number 4.0)]) ~?= Right True,
        validate2 validateObj2 testOP (Object $ H [("bool", Boolean False), ("num", Number 4.0)]) ~?= Left "bool| boolean is not the provided enum",
        validate2 validateObj2 testOP (Object $ H [("bool", Boolean True), ("Bill", Null), ("Jim", Null)]) ~?= Right True,
        validate2 validateObj2 testOP (Object $ H [("bool", Boolean True), ("Bill", Number 4.0), ("Jim", Null), ("Mike", Null), ("George", Null), ("Chris", Null)]) ~?= Left "| too many properties in given object",
        validate2 validateObj2 testOP (Object $ H [("bool", Boolean True), ("null", Null)]) ~?= Right True,
        validate2 validateObj2 testOP (Object $ H [("int", Integer 2), ("null", Null)]) ~?= Left "bool| doesn't exist",
        validate2 validateObj2 testOP (Object $ H [("int", Integer 2), ("null", Number 4.0), ("bool", Boolean True)]) ~?= Left "null| is not null but should be",
        validate2 validateObj2 testOP (Object $ H [("bool", Boolean True), ("int", Integer 2), ("null", Null), ("num", Number 4.0), ("str", String "aaaaaab")]) ~?= Right True,
        validate2 validateObj2 testOP (Object $ H [("bool", Boolean True), ("arr", Array [Integer 2, Integer 4])]) ~?= Right True,
        validate2 validateObj2 testOP (Object $ H [("bool", Boolean True), ("obj", Object $ H [("bool", Boolean True), ("num", Number 4.0)])]) ~?= Right True,
        validate2 validateObj2 testOP (Object $ H [("bool", Boolean True), ("obj", Object $ H [("bool", Boolean False), ("num", Number 4.0)])]) ~?= Left "obj.bool| boolean is not the provided enum",
        validate2 validateObj2 testOP Null ~?= Left "| provided value is not an object"
      ]

-- >>> validate2 validateObj2 testOP Null
-- Left "provided value is not an array"

--- >>> runTestTT test_validateObj

-------------------------- HSON Validation Test ------------------------------------

tValidateHSON :: Test
tValidateHSON =
  "parse valid json"
    ~: TestList
      [ "address" ~: p "test/json-schema/schema/address-schema.json" "test/json-schema/object/address-object.json",
        "coordinate" ~: p "test/json-schema/schema/coordinate-schema.json" "test/json-schema/object/coordinate-object.json",
        "card" ~: p "test/json-schema/schema/card-schema.json" "test/json-schema/object/card-object.json"
      ]
  where
    p schema obj = do
      s <- parseJSON schema
      o <- parseJSON obj
      case (s, o) of
        (Right x, Right y) -> do
          assert (isRight $ validateHSON2 y (Maybe.fromJust $ hsonToHSONSchema x))
        (_, _) -> assert False

-- >>> runTestTT tValidateHSON
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}

test_validateSchemas :: IO Counts
test_validateSchemas =
  runTestTT $
    TestList
      [ test_maybeValidate,
        test_uniqueElems,
        test_validateNum2,
        test_validateInt2,
        test_validateString2,
        test_validateBool2,
        test_validateArr2,
        test_validateObj2,
        tValidateHSON
      ]

-- >>> test_validateSchemas
-- Counts {cases = 54, tried = 54, errors = 0, failures = 0}
