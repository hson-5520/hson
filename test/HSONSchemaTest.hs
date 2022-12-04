import Control.Applicative
import Data.Map qualified as Map
import FromJSON
import HSON (HSON, Key, Value (Array, Boolean, Null, Number, Object, String), hsonArray, hsonDog, hsonEmpty, hsonSchool, hsonSingle)
import HSONSchema (HSONSchema, address, card, coordinate, hsonToHSONSchema)
import Lib
import Parser qualified as P
import Test.HUnit (Test (TestList), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck
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
      [ "address" ~: p "../test/json-schema/schema/address-schema.json" address,
        "card" ~: p "../test/json-schema/schema/card-schema.json" card,
        "coordinate" ~: p "../test/json-schema/schema/coordinate-schema.json" coordinate
      ]
  where
    p fn hschema = do
      z <- parseJSON fn
      case z of
        (Left _) -> assert False
        (Right ast') -> assert (hschema == hsonToHSONSchema ast')
