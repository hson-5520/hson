module ToJSONTest where

import Control.Applicative
import Data.Map qualified as Map
import FromJSON
import HSON
  ( HSON (H),
    Key,
    Value (Array, Boolean, Integer, Null, Number, Object, String),
  )
import Parser qualified as P
import Test.HUnit (Counts, Test (TestList), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck
import ToJSON

-------------------------- Writing Keys Tests ----------------------------------

test_keyToString :: Test
test_keyToString =
  "writing string key test"
    ~: TestList
      [ keyToString "a" ~?= "\"a\": ",
        keyToString "" ~?= "\"\": ",
        keyToString "bil32" ~?= "\"bil32\": "
      ]

--- >>> runTestTT test_keyToString

-------------------------- Writing Values Tests --------------------------------
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

test_nullToString :: Test
test_nullToString =
  "writing null value test"
    ~: TestList
      [ nullToString ~?= "null"
      ]

--- >>> runTestTT test_nullToString

---------------------------- Sample HSON ---------------------------------------
hsonEmpty :: HSON
hsonEmpty = H []

hsonSingle :: HSON
hsonSingle = H [("name", String "bob")]

hsonArray :: HSON
hsonArray =
  H
    [ ( "bob",
        Array
          [ Integer 1,
            String "hi",
            Object $ H [("name", String "Jose")],
            Null
          ]
      )
    ]

hsonDog :: HSON
hsonDog =
  H
    [ ( "dog",
        Object $
          H
            [ ("name", String "bill"),
              ("age", Number 4.2),
              ("siblings", Boolean False)
            ]
      )
    ]

hsonSchool :: HSON
hsonSchool =
  H
    [ ("name", String "School"),
      ("foundedYear", Integer 1975),
      ("isPublic", Boolean True),
      ("cost", Null),
      ("students", Array [String "a", String "b", String "c"]),
      ( "address",
        Object $
          H
            [ ("city", String "Philadelphia"),
              ("state", String "Pennsylvania"),
              ("buildingNumber", Integer 123)
            ]
      )
    ]

-------------------------- Create JSON Tests -----------------------------------

compareFiles :: FilePath -> FilePath -> IO Bool
compareFiles f1 f2 = do
  aContents <- parseJSON f1
  bContents <- parseJSON f2
  case (aContents, bContents) of
    (Right x, Right y) -> return (x == y)
    _ -> return False

-- >>> runTestTT tParseValidJson

tParseValidJson :: Test
tParseValidJson =
  "parse valid json"
    ~: TestList
      [ "empty"
          ~: p
            "test/json/valid/empty.json"
            "test/json/test/empty.json"
            hsonEmpty,
        "single"
          ~: p
            "test/json/valid/single.json"
            "test/json/test/single.json"
            hsonSingle,
        "array"
          ~: p
            "test/json/valid/array.json"
            "test/json/test/array.json"
            hsonArray,
        "dog"
          ~: p
            "test/json/valid/dog.json"
            "test/json/test/dog.json"
            hsonDog,
        "school"
          ~: p
            "test/json/valid/school.json"
            "test/json/test/school.json"
            hsonSchool
      ]
  where
    p fn fp hson = do
      toJSON fp hson
      x <- compareFiles fn fp
      assert x

------------------------------- All Tests --------------------------------------

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

--------------------------------------------------------------------------------
