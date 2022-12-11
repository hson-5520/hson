import Control.Applicative
import Data.Map qualified as Map
import FromJSON
import GHC.Generics (Generic1 (from1))
import HSON (HSON, Key, Value (Array, Boolean, Integer, Null, Number, Object, String), hsonArray, hsonDog, hsonEmpty, hsonSchool, hsonSingle)
import HSONSchema (IntProperties (IP, iExclusiveMaximum, iExclusiveMinimum, iMaximum, iMinimum, iMultipleOf, intEnum), NumProperties (NP, nExclusiveMaximum, nExclusiveMinimum, nMaximum, nMinimum, nMultipleOf, numberEnum), address, card, coordinate)
import HSONValidator
import HSONValidator (Schema, maybeValidate)
import Lib
import Parser qualified as P
import Test.HUnit (Test (TestList), assert, runTestTT, (~:), (~?=))
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
-- Counts {cases = 7, tried = 7, errors = 0, failures = 0}

test_validateString :: Test
test_validateString = undefined

--- >>> runTestTT test_validateString

test_validateArr :: Test
test_validateArr = undefined

--- >>> runTestTT test_validateArr

test_validateObj :: Test
test_validateObj = undefined

--- >>> runTestTT test_validateObj

-------------------------- HSON Validation Test ------------------------------------

-- tParseValidJson :: Test
-- tParseValidJson =
--   "parse valid json"
--     ~: TestList
--       [ "address" ~: p "../test/json-schema/schema/address-schema.json" "../test/json-schema/object/address-object.json",
--         "coordinate" ~: p "../test/json-schema/schema/coordinate-schema.json" "../test/json-schema/object/coordinate-object.json",
--         "card" ~: p "../test/json-schema/schema/card-schema.json" "../test/json-schema/object/card-object.json"
--       ]
--   where
--     p schema obj = do
--       s <- parseJSON schema
--       o <- parseJSON obj
--       case (s, o) of
--         (Right x, Right y) -> assert (validateHSON y (hsonToHSONSchema x))
--         (_, _) -> assert False
