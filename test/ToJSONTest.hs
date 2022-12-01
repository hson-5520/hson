import Control.Applicative
import Data.Map qualified as Map
import HSON (HSON (Empty, Map), Key, Value (Array, Boolean, Null, Number, Object, String), hsonArray, hsonDog, hsonEmpty, hsonSchool, hsonSingle)
import Lib
import Parser qualified as P
import Test.HUnit (Test (TestList), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck
import ToJSON

------------------------- Writing Keys Tests -----------------------------

test_keyToString :: Test
test_keyToString =
  "writing string key test"
    ~: TestList
      [ keyToString "a" ~?= "\"a\":",
        keyToString "" ~?= "\"\":",
        keyToString "bil32" ~?= "\"bil32\":"
      ]

--- >>> runTestTT test_keyToString

-------------------------- Writing Values Tests -----------------------------------------

test_stringToString :: Test
test_stringToString =
  "writing string value test"
    ~: TestList
      [ stringToString "a" ~?= "\"a\"",
        stringToString "Yathu" ~?= "\"Yathu\""
      ]

--- >>> runTestTT test_stringToString

test_numberToString :: Test
test_numberToString =
  "writing number value test"
    ~: TestList
      [ numberToString 1 ~?= "1",
        numberToString (-1) ~?= "-1",
        numberToString 2953.40 ~?= "2953.40",
        numberToString (-2953.40) ~?= "-2953.40"
      ]

--- >>> runTestTT test_numberToString

test_booleanToString :: Test
test_booleanToString =
  "writing boolean value test"
    ~: TestList
      [ booleanToString True ~?= "true",
        booleanToString False ~?= "false"
      ]

--- >>> runTestTT test_booleanToString

test_arrayToString :: Test
test_arrayToString =
  "writing array value test"
    ~: TestList
      [ arrayToString [Number 1, Number 2, Number 3] ~?= "[1, 2, 3]",
        arrayToString [] ~?= "[]"
      ]

--- >>> runTestTT test_arrayToString

test_objectToString :: Test
test_objectToString =
  "writing object value test"
    ~: TestList
      [ objectToString Empty ~?= "{}",
        objectToString (Map $ Map.fromList [("bill", Number 1)]) ~?= "{ \"bill\": 1 }"
      ]

--- >>> runTestTT test_objectToString

test_nullToString :: Test
test_nullToString =
  "writing null value test"
    ~: TestList
      [ nullToString ~?= "null"
      ]

--- >>> runTestTT test_nullToString
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}

-------------------------- Create JSON Tests ------------------------------------

-- Inspired By:
-- https://stackoverflow.com/questions/16952335/is-there-any-way-to-use-io-bool-in-if-statement-without-binding-to-a-name-in-has
compareFiles :: FilePath -> FilePath -> IO Bool
compareFiles f1 f2 = do
  aContents <- readFile f1
  bContents <- readFile f1
  return (aContents == bContents)

tParseValidJson :: Test
tParseValidJson =
  "parse valid json"
    ~: TestList
      [ "empty" ~: p "../test/json/valid/empty.json" "emptyfile.json" hsonEmpty,
        "single" ~: p "../test/json/valid/single.json" "singlefile.json" hsonSingle,
        "array" ~: p "../test/json/valid/array.json" "arrayfile.json" hsonArray,
        "dog" ~: p "../test/json/valid/dog.json" "dogfile.json" hsonDog,
        "school" ~: p "../test/json/valid/school.json" "schoolfile.json" hsonSchool
      ]
  where
    p fn fp hson = do
      toJSON fp hson
      x <- compareFiles fn fp
      assert x
