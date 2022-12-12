module HSONValidatorTest where

import Control.Applicative
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import FromJSON
import GHC.Generics (Generic1 (from1))
import HSON (HSON (..), Key, Value (Array, Boolean, Integer, Null, Number, Object, String), hsonArray, hsonDog, hsonEmpty, hsonSchool, hsonSingle)
import HSONSchema (ArrProperties (..), BoolProperties (..), HSONSchema (..), IntProperties (..), NumProperties (..), ObjProperties (..), StrProperties (..), address, card, coordinate)
import HSONValidator
import Lib
import Parser qualified as P
import Test.HUnit (Counts, Test (TestList), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck
import ToHSONSchema (hsonToHSONSchema)
import ToJSON

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
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}

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

--- >>> runTestTT test_validateNum
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

--- >>> runTestTT test_validateInt

testSP :: StrProperties
testSP =
  SP
    { minLength = Just 5,
      maxLength = Just 10,
      pattern = Nothing,
      stringEnum = Just ["a", "aaaaaa", "aaaaaaa"]
    }

test_validateString :: Test
test_validateString =
  "test validate string"
    ~: TestList
      [ validate validateString testSP (String "aaaaaa") ~?= True,
        validate validateString testSP (String "aaaaaaa") ~?= True,
        validate validateString testSP (String "hello") ~?= False,
        validate validateString testSP (String "a") ~?= False,
        validate validateString testSP (String "this is too long") ~?= False,
        validate validateString testSP Null ~?= False
      ]

--- >>> runTestTT test_validateString
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

--- >>> runTestTT test_validateBool
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

--- >>> runTestTT test_validateArr

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
        validate validateObj testOP (Object $ H [("bool", Boolean True), ("int", Integer 2), ("null", Null), ("num", Number 4.0), ("str", String "aaaaaa")]) ~?= True,
        validate validateObj testOP (Object $ H [("bool", Boolean True), ("arr", Array [Integer 2, Integer 4])]) ~?= True,
        validate validateObj testOP (Object $ H [("bool", Boolean True), ("obj", Object $ H [("bool", Boolean True), ("num", Number 4.0)])]) ~?= True,
        validate validateObj testOP (Object $ H [("bool", Boolean True), ("obj", Object $ H [("bool", Boolean False), ("num", Number 4.0)])]) ~?= False,
        validate validateObj testOP Null ~?= False
      ]

--- >>> runTestTT test_validateObj
-- Counts {cases = 13, tried = 13, errors = 0, failures = 0}

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
          assert (validateHSON y (Maybe.fromJust $ hsonToHSONSchema x))
        (_, _) -> assert False

-- >>> runTestTT tValidateHSON
-- Counts {cases = 3, tried = 3, errors = 0, failures = 1}

test_validateSchemas :: IO Counts
test_validateSchemas =
  runTestTT $
    TestList
      [ test_maybeValidate,
        test_uniqueElems,
        test_validateNum,
        test_validateInt,
        test_validateString,
        test_validateBool,
        test_validateArr,
        test_validateObj,
        tValidateHSON
      ]

-- >>> test_validateSchemas
-- Counts {cases = 54, tried = 54, errors = 0, failures = 0}
