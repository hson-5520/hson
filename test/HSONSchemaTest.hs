module HSONSchemaTest (test_validation) where

import Control.Applicative
import Data.Map qualified as Map
import FromJSON
import HSON (HSON (H), Key, Value (Array, Boolean, Null, Number, Object, String), hsonArray, hsonDog, hsonEmpty, hsonSchool, hsonSingle)
import HSONSchema (HSONSchema, address, card, coordinate)
import Lib
import Parser qualified as P
import Test.HUnit (Counts, Test (TestList), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck
import ToHSONSchema (hsonToHSONSchema, objHelper)
import ToJSON

------------------------- HSON Schema PropertyTests -----------------------------

test_numberHelper :: Test
test_numberHelper = undefined

--- >>> runTestTT test_numberHelper

test_intHelper :: Test
test_intHelper = undefined

--- >>> runTestTT test_intHelper

test_stringHelper :: Test
test_stringHelper = undefined

--- >>> runTestTT test_stringHelper

test_boolHelper :: Test
test_boolHelper = undefined

--- >>> runTestTT test_boolHelper

test_arrHelper :: Test
test_arrHelper = undefined

--- >>> runTestTT test_arrHelper

test_objHelper :: Test
test_objHelper = undefined

--- >>> runTestTT test_numberHelper

-------------------------- Create HSON Schema Tests ------------------------------------

tCreateHSONSchema :: Test
tCreateHSONSchema =
  "create HSON schema"
    ~: TestList
      [ "address" ~: p "../test/json-schema/schema/address-schema.json" (Just address),
        "card" ~: p "../test/json-schema/schema/card-schema.json" (Just card),
        "coordinate" ~: p "../test/json-schema/schema/coordinate-schema.json" (Just card)
      ]
  where
    p fn hschema = do
      z <- parseJSON fn
      case z of
        (Left _) -> assert False
        (Right ast') -> assert (hschema == hsonToHSONSchema ast')

-- >>> runTestTT tCreateHSONSchema
-- Counts {cases = 3, tried = 3, errors = 0, failures = 3}

-- >>> address2
-- Variable not in scope: address2

-- >>> hsonToHSONSchema (parseJSON address)

address3 :: IO (Either P.ParseError HSON)
address3 = do
  parseJSON "test/json-schema/schema/coordinate-schema.json"

-- >>> address3

address5 :: IO (Maybe HSONSchema)
address5 = do
  z <- parseJSON "test/json-schema/schema/coordinate-schema.json"
  case z of
    Right x -> return $ objHelper x
    Left z -> return Nothing

-- >>> address5

test_validation :: IO Counts
test_validation =
  runTestTT $
    TestList
      [ tCreateHSONSchema
      ]
