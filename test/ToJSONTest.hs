module ToJSONTest where

import Control.Applicative
import Data.Map qualified as Map
import HSON (HSON (H), Key, Value (Array, Boolean, Integer, Null, Number, Object, String), hsonArray, hsonDog, hsonEmpty, hsonSchool, hsonSingle)
import Lib
import Parser qualified as P
import Test.HUnit (Counts, Test (TestList), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck
import ToJSON

------------------------- Writing Keys Tests -----------------------------

test_keyToString :: Test
test_keyToString =
  "writing string key test"
    ~: TestList
      [ keyToString "a" ~?= "\"a\": ",
        keyToString "" ~?= "\"\": ",
        keyToString "bil32" ~?= "\"bil32\": "
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

test_integerToString :: Test
test_integerToString =
  "writing number value test"
    ~: TestList
      [ integerToString 1 ~?= "1",
        integerToString (-1) ~?= "-1"
      ]

--- >>> runTestTT test_integerToString
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}

test_numberToString :: Test
test_numberToString =
  "writing number value test"
    ~: TestList
      [ numberToString 2953.48 ~?= "2953.48",
        numberToString (-2953.47) ~?= "-2953.47"
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
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}

test_arrayToString :: Test
test_arrayToString =
  "writing array value test"
    ~: TestList
      [ arrayToString [Number 1.2, Number 2.5, Integer 3] ~?= "[1.2, 2.5, 3]",
        arrayToString [Null] ~?= "[null]",
        arrayToString [] ~?= "[]"
      ]

--- >>> runTestTT test_arrayToString

test_objectToString :: Test
test_objectToString =
  "writing object value test"
    ~: TestList
      [ objectToString (H []) ~?= "{}",
        objectToString (H [("bill", Integer 1)]) ~?= "{\"bill\": 1}"
      ]

--- >>> runTestTT test_objectToString
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}

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
  bContents <- readFile f2
  return (aContents == bContents)

-- >>> compareFiles "arrayfile.json" "test/json/valid/array.json"
-- True

-- >>> compareFiles "dogfile.json" "test/json/valid/dog.json"
-- True

-- >>> compareFiles "emptyfile.json" "test/json/valid/empty.json"
-- True

-- >>> compareFiles "schoolfile.json" "test/json/valid/school.json"
-- True

-- >>> compareFiles "singlefile.json" "test/json/valid/single.json"
-- True

-- >>> runTestTT tParseValidJson
-- Counts {cases = 5, tried = 5, errors = 4, failures = 0}

tParseValidJson :: Test
tParseValidJson =
  "parse valid json"
    ~: TestList
      [ "empty" ~: p "test/json/valid/empty.json" "emptyfile.json" hsonEmpty,
        "single" ~: p "test/json/valid/single.json" "singlefile.json" hsonSingle,
        "array" ~: p "test/json/valid/array.json" "arrayfile.json" hsonArray,
        "dog" ~: p "test/json/valid/dog.json" "dogfile.json" hsonDog,
        "school" ~: p "test/json/valid/school.json" "schoolfile.json" hsonSchool
      ]
  where
    p fn fp hson = do
      toJSON fp hson
      x <- compareFiles fn fp
      assert x

test_toJSON :: IO Counts
test_toJSON =
  runTestTT $
    TestList
      [ test_keyToString,
        test_stringToString,
        test_integerToString,
        test_numberToString,
        test_booleanToString,
        test_arrayToString,
        test_objectToString,
        test_nullToString,
        tParseValidJson
      ]

-- >>> test_toJSON
-- Counts {cases = 17, tried = 17, errors = 0, failures = 0}
