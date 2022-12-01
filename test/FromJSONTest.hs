import Control.Applicative
import Data.Map qualified as Map
import FromJSON
import HSON (HSON (Empty, Map), Key, Value (Array, Boolean, Null, Number, Object, String), hsonArray, hsonDog, hsonEmpty, hsonSchool, hsonSingle)
import Lib
import Parser qualified as P
import Test.HUnit (Test (TestList), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck

------------------------- Parsing Primatives Tests -----------------------------

test_wsP :: Test
test_wsP =
  TestList
    [ P.parse (wsP P.alpha) "a" ~?= Right 'a',
      P.parse (many (wsP P.alpha)) "a b \n   \t c" ~?= Right "abc"
    ]

--- >>> runTestTT test_wsP

test_stringP :: Test
test_stringP =
  TestList
    [ P.parse (stringP "a") "a" ~?= Right (),
      P.parse (stringP "a") "b" ~?= Left "No parses",
      P.parse (many (stringP "a")) "a  a" ~?= Right [(), ()]
    ]

-- >>> runTestTT test_stringP

test_constP :: Test
test_constP =
  TestList
    [ P.parse (constP "&" 'a') "&  " ~?= Right 'a',
      P.parse (many (constP "&" 'a')) "&   &" ~?= Right "aa"
    ]

-- >>> runTestTT test_constP

-------------------------- Key Parsing Tests -----------------------------------------

test_keyP :: Test
test_keyP =
  "parse json key"
    ~: TestList
      [ P.parse keyP "\"a\":" ~?= Right "a",
        P.parse keyP "\"a\"" ~?= Left "No parses"
      ]

-- >>> runTestTT test_keyP
-------------------------- Value Parsing Tests -----------------------------------------

test_stringValP :: Test
test_stringValP =
  "parse json string value "
    ~: TestList
      [ P.parse stringValP "\"a\"" ~?= Right (String "a"),
        P.parse stringValP "\"a\\\"\"" ~?= Right (String "a\\"),
        P.parse (many stringValP) "\"a\"   \"b\"" ~?= Right [String "a", String "b"],
        P.parse (many stringValP) "\" a\"   \"b\"" ~?= Right [String " a", String "b"]
      ]

-- >>> runTestTT test_stringValP

test_numberValP :: Test
test_numberValP =
  "parse json number value "
    ~: TestList
      [ P.parse numberValP "\"a\"" ~?= Left "no parses",
        P.parse numberValP "23" ~?= Right (Number 23)
      ]

-- >>> runTestTT test_numberValP

test_booleanValP :: Test
test_booleanValP =
  "parse json number value "
    ~: TestList
      [ P.parse booleanValP "\"a\"" ~?= Left "no parses",
        P.parse booleanValP "\"true\"" ~?= Right (Boolean True),
        P.parse booleanValP "\"false\"" ~?= Right (Boolean False)
      ]

-- >>> runTestTT test_booleanValP

test_arrayValP :: Test
test_arrayValP =
  "parse json array value "
    ~: TestList
      [ P.parse arrayValP "\"a\"" ~?= Left "no parses",
        P.parse arrayValP "[1, 2, 3]" ~?= Right (Array [Number 1, Number 2, Number 3])
      ]

-- >>> runTestTT test_arrayValP

test_objectValP :: Test
test_objectValP =
  "parse json object value "
    ~: TestList
      [ P.parse objectValP "\"a\"" ~?= Left "no parses",
        P.parse objectValP "\"{}\"" ~?= Right (Object (Map Map.empty))
      ]

-- >>> runTestTT test_objectValP

test_nullValP :: Test
test_nullValP =
  "parse json null value "
    ~: TestList
      [ P.parse nullValP "\"a\"" ~?= Left "no parses",
        P.parse nullValP "null" ~?= Right Null
      ]

-- >>> runTestTT test_nullValP

-------------------------- Parse JSON Tests ------------------------------------

tParseValidJson :: Test
tParseValidJson =
  "parse valid json"
    ~: TestList
      [ "empty" ~: p "../test/json/valid/empty.json" hsonEmpty,
        "single" ~: p "../test/json/valid/single.json" hsonSingle,
        "array" ~: p "../test/json/valid/array.json" hsonArray,
        "dog" ~: p "../test/json/valid/dog.json" hsonDog,
        "school" ~: p "../test/json/valid/school.json" hsonSchool
      ]
  where
    p fn ast = do
      result <- parseJSON fn
      case result of
        (Left _) -> assert False
        (Right ast') -> assert (ast == ast')

tParseInvalidJson :: Test
tParseInvalidJson =
  "parse invalid json"
    ~: TestList
      [ "boolean" ~: p "../test/json/invalid/boolean.txt",
        "bracket" ~: p "../test/json/invalid/bracket.txt",
        "comma" ~: p "../test/json/invalid/comma.txt",
        "number" ~: p "../test/json/invalid/number.txt"
      ]
  where
    p fn = do
      result <- parseJSON fn
      case result of
        (Left _) -> assert True
        _ -> assert False
