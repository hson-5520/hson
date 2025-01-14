module FromJSONTest where

import Control.Applicative
import FromJSON
import HSON
  ( HSON (H),
    Key,
    Value (Array, Boolean, Integer, Null, Number, Object, String),
  )
import Parser qualified as P
import Test.HUnit (Counts, Test (TestList), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck ()

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

-------------------------- Key Parsing Tests -----------------------------------

test_keyP :: Test
test_keyP =
  "parse json key"
    ~: TestList
      [ P.parse keyP "\"a\":" ~?= Right "a",
        P.parse keyP "\"a\"" ~?= Left "No parses"
      ]

-- >>> runTestTT test_keyP
-------------------------- Value Parsing Tests ---------------------------------

test_stringValP :: Test
test_stringValP =
  "parse json string value "
    ~: TestList
      [ P.parse stringValP "\"a\"" ~?= Right (String "a"),
        P.parse stringValP "\"a\\\"\"" ~?= Right (String "a\\"),
        P.parse (many stringValP) "\"a\"   \"b\""
          ~?= Right [String "a", String "b"],
        P.parse (many stringValP) "\" a\"   \"b\""
          ~?= Right [String " a", String "b"]
      ]

-- >>> runTestTT test_stringValP

test_intValP :: Test
test_intValP =
  "parse json int value "
    ~: TestList
      [ P.parse intValP "\"a\"" ~?= Left "No parses",
        P.parse intValP "23" ~?= Right (Integer 23),
        P.parse intValP "-23" ~?= Right (Integer (-23))
      ]

-- >>> runTestTT test_intValP

test_numberValP :: Test
test_numberValP =
  "parse json number value "
    ~: TestList
      [ P.parse numberValP "\"a\"" ~?= Left "No parses",
        P.parse numberValP "12.55" ~?= Right (Number 12.55),
        P.parse numberValP "-12.55" ~?= Right (Number (-12.55)),
        P.parse numberValP "204.45e5" ~?= Right (Number 2.0445e7),
        P.parse numberValP "-204.45E5" ~?= Right (Number (-2.0445e7)),
        P.parse numberValP "204.45e-5" ~?= Right (Number 2.0445e-3),
        P.parse numberValP "-204.45e-5" ~?= Right (Number (-2.0445e-3)),
        P.parse numberValP "2e-2" ~?= Right (Number 0.02),
        P.parse numberValP "2e+2" ~?= Right (Number 200.0),
        P.parse numberValP "2e2" ~?= Right (Number 200.0),
        P.parse numberValP "2E+2" ~?= Right (Number 200.0)
      ]

-- >>> runTestTT test_numberValP

test_booleanValP :: Test
test_booleanValP =
  "parse json number value "
    ~: TestList
      [ P.parse booleanValP "\"a\"" ~?= Left "No parses",
        P.parse booleanValP "true" ~?= Right (Boolean True),
        P.parse booleanValP "false" ~?= Right (Boolean False),
        P.parse booleanValP "True" ~?= Left "No parses",
        P.parse booleanValP "False" ~?= Left "No parses"
      ]

-- >>> runTestTT test_booleanValP

test_arrayValP :: Test
test_arrayValP =
  "parse json array value "
    ~: TestList
      [ P.parse arrayValP "\"a\"" ~?= Left "No parses",
        P.parse arrayValP "[1, 2, 3.1]"
          ~?= Right
            ( Array
                [ Integer 1,
                  Integer 2,
                  Number 3.1
                ]
            ),
        P.parse arrayValP "[32, \"america\", null, {\"bob\": -2.1}]"
          ~?= Right
            ( Array
                [ Integer 32,
                  String "america",
                  Null,
                  Object $ H [("bob", Number (-2.1))]
                ]
            )
      ]

-- >>> runTestTT test_arrayValP

test_objectValP :: Test
test_objectValP =
  "parse json object value "
    ~: TestList
      [ P.parse objectValP "\"a\"" ~?= Left "No parses",
        P.parse objectValP "{}"
          ~?= Right
            (Object $ H []),
        P.parse
          objectValP
          "{ \"this\": 12 }"
          ~?= Right (Object $ H [("this", Integer 12)]),
        P.parse
          objectValP
          "{ \"this\": 12 , \"that\": true }"
          ~?= Right (Object $ H [("this", Integer 12), ("that", Boolean True)])
      ]

-- >>> runTestTT test_objectValP

test_nullValP :: Test
test_nullValP =
  "parse json null value "
    ~: TestList
      [ P.parse nullValP "\"a\"" ~?= Left "No parses",
        P.parse nullValP "null" ~?= Right Null,
        P.parse nullValP "Null" ~?= Left "No parses"
      ]

-- >>> runTestTT test_nullValP

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

-------------------------- Parse JSON Tests ------------------------------------

tParseValidJson :: Test
tParseValidJson =
  "parse valid json"
    ~: TestList
      [ "empty" ~: p "test/json/valid/empty.json" hsonEmpty,
        "single" ~: p "test/json/valid/single.json" hsonSingle,
        "array" ~: p "test/json/valid/array.json" hsonArray,
        "dog" ~: p "test/json/valid/dog.json" hsonDog,
        "school" ~: p "test/json/valid/school.json" hsonSchool
      ]
  where
    p fn ast = do
      result <- parseJSON fn
      case result of
        (Left _) -> assert False
        (Right ast') -> assert (ast == ast')

-- >>> runTestTT tParseValidJson

tParseInvalidJson :: Test
tParseInvalidJson =
  "parse invalid json"
    ~: TestList
      [ "boolean" ~: p "test/json/invalid/boolean.txt",
        "bracket" ~: p "test/json/invalid/bracket.txt",
        "comma" ~: p "test/json/invalid/comma.txt",
        "number" ~: p "test/json/invalid/number.txt"
      ]
  where
    p fn = do
      result <- parseJSON fn
      case result of
        (Left _) -> assert True
        _ -> assert False

------------------------------- All Tests --------------------------------------

test_fromJSON :: IO Counts
test_fromJSON =
  runTestTT $
    TestList
      [ test_wsP,
        test_stringP,
        test_constP,
        test_keyP,
        test_stringValP,
        test_intValP,
        test_numberValP,
        test_booleanValP,
        test_arrayValP,
        test_objectValP,
        test_nullValP,
        tParseValidJson,
        tParseInvalidJson
      ]

-- >>> test_fromJSON

--------------------------------------------------------------------------------
