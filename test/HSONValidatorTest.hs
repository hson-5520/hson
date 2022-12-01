import Control.Applicative
import Data.Map qualified as Map
import FromJSON
import GHC.Generics (Generic1 (from1))
import HSON (HSON (Empty, Map), Key, Value (Array, Boolean, Null, Number, Object, String), hsonArray, hsonDog, hsonEmpty, hsonSchool, hsonSingle)
import HSONSchema (address, card, coordinate, hsonToHSONSchema)
import HSONValidator
import Lib
import Parser qualified as P
import Test.HUnit (Test (TestList), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck
import ToJSON

------------------------- HSON Schema Property Validating Tests -----------------------------

test_validateNum :: Test
test_validateNum = undefined

--- >>> runTestTT test_validateNum

test_validateInt :: Test
test_validateInt = undefined

--- >>> runTestTT test_validateInt

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

tParseValidJson :: Test
tParseValidJson =
  "parse valid json"
    ~: TestList
      [ "address" ~: p "../test/json-schema/schema/address-schema.json" "../test/json-schema/object/address-object.json",
        "coordinate" ~: p "../test/json-schema/schema/coordinate-schema.json" "../test/json-schema/object/coordinate-object.json",
        "card" ~: p "../test/json-schema/schema/card-schema.json" "../test/json-schema/object/card-object.json"
      ]
  where
    p schema obj = do
      s <- parseJSON schema
      o <- parseJSON obj
      case (s, o) of
        (Right x, Right y) -> assert (validateHSON y (hsonToHSONSchema x))
        (_, _) -> assert False
