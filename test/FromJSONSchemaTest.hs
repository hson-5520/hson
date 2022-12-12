module FromJSONSchemaTest where

import Control.Applicative
import Data.Bits (Bits (xor))
import Data.Bool (bool)
import Data.Map qualified as Map
import Data.Maybe
import FromJSON
import FromJSONSchema (arrHelper, boolArrayHelper, boolHelper, checkArrayLength, filterBoolArray, filterIntArray, filterNumberArray, filterStringArray, getProperties, hsonToHSONSchema, intHelper, matchBool, matchInt, matchNumber, matchString, numberHelper, objHelper, schemaParser, stringHelper)
import HSON (HSON (H), Key, Value (Array, Boolean, Integer, Null, Number, Object, String), hsonArray, hsonDog, hsonEmpty, hsonSchool, hsonSingle)
import HSONSchema (ArrProperties (AP, isUnique, items, maxItems, minItems), BoolProperties (BP, boolEnum), HSONSchema (Arr, Bool, Int, Num, Obj, Str), IntProperties (IP, iExclusiveMaximum, iExclusiveMinimum, iMaximum, iMinimum, iMultipleOf, intEnum), NumProperties (NP, nExclusiveMaximum, nExclusiveMinimum, nMaximum, nMinimum, nMultipleOf, numberEnum), ObjProperties (OP, maxProperties, minProperties, properties, required), StrProperties (SP, maxLength, minLength, pattern, stringEnum), address, card, coordinate)
import Parser qualified as P
import Test.HUnit (Counts, Test (TestList), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck
import ToJSON

------------------------- ToHSONSchema Helpers' Tests  --------------------------------

test_matchInt :: Test
test_matchInt =
  "match int"
    ~: TestList
      [ matchInt "test" (Map.fromList [("test", Integer 2)]) ~?= (True, Just 2),
        matchInt "test" Map.empty ~?= (True, Nothing),
        matchInt "test" (Map.fromList [("randomKey", Boolean True)]) ~?= (True, Nothing),
        matchInt "test" (Map.fromList [("randomKey", Boolean True), ("test", Integer 2)]) ~?= (True, Just 2),
        matchInt "test" (Map.fromList [("randomKey", Boolean True), ("test", String "abcVal")]) ~?= (False, Nothing)
      ]

-- >>> runTestTT test_matchInt
-- Counts {cases = 5, tried = 5, errors = 0, failures = 0}

test_matchNumber :: Test
test_matchNumber =
  "match number"
    ~: TestList
      [ matchNumber "test" (Map.fromList [("test", Number 2.0)]) ~?= (True, Just 2.0),
        matchNumber "test" Map.empty ~?= (True, Nothing),
        matchNumber "test" (Map.fromList [("randomKey", String "True")]) ~?= (True, Nothing),
        matchNumber "test" (Map.fromList [("randomKey", Boolean True), ("test", Number 5.0)]) ~?= (True, Just 5.0),
        matchNumber "test" (Map.fromList [("randomKey", Boolean True), ("test", Integer 4)]) ~?= (True, Just 4.0)
      ]

-- >>> runTestTT test_matchNumber

test_matchBool :: Test
test_matchBool =
  "match bool"
    ~: TestList
      [ matchBool "boolKey" (Map.fromList [("boolKey", Boolean True)]) ~?= (True, Just True),
        matchBool "boolKey" Map.empty ~?= (True, Nothing),
        matchBool "boolKey" (Map.fromList [("randomKey", String "True")]) ~?= (True, Nothing),
        matchBool "boolKey" (Map.fromList [("randomKey", Boolean True), ("boolKey", Boolean False)]) ~?= (True, Just False),
        matchBool "boolKey" (Map.fromList [("randomKey", Boolean True), ("boolKey", Integer 4)]) ~?= (False, Nothing)
      ]

-- >>> runTestTT test_matchBool
-- Counts {cases = 5, tried = 5, errors = 0, failures = 0}

test_matchString :: Test
test_matchString =
  "match string"
    ~: TestList
      [ matchString "test" (Map.fromList [("test", String "hello world")]) ~?= (True, Just "hello world"),
        matchString "test" Map.empty ~?= (True, Nothing),
        matchString "test" (Map.fromList [("randomKey", String "True")]) ~?= (True, Nothing),
        matchString "test" (Map.fromList [("randomKey", Boolean True), ("test", String "hello")]) ~?= (True, Just "hello"),
        matchString "test" (Map.fromList [("randomKey", Boolean True), ("test", Integer 4)]) ~?= (False, Nothing)
      ]

-- >>> runTestTT test_matchString
-- Counts {cases = 5, tried = 5, errors = 0, failures = 0}

test_valueArray :: [Value]
test_valueArray = [Integer 1, Number 2.0, Boolean True, String "hello", Integer 3, Number 4.0, Boolean False, String "world"]

test_filterIntArray :: Test
test_filterIntArray =
  "filter int array"
    ~: TestList
      [ filterIntArray test_valueArray ~?= [1, 3],
        filterIntArray [Integer 5, Integer 7, Integer 9] ~?= [5, 7, 9],
        filterIntArray [] ~?= []
      ]

-- >>> runTestTT test_filterIntArray
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}

test_filterNumberArray :: Test
test_filterNumberArray =
  "filter number array"
    ~: TestList
      [ filterNumberArray test_valueArray ~?= [2.0, 4.0],
        filterNumberArray [Number 5.1, Number 7.2, Number 9.3] ~?= [5.1, 7.2, 9.3],
        filterNumberArray [] ~?= []
      ]

--- >>> runTestTT test_filterNumberArray
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}

test_filterBoolArray :: Test
test_filterBoolArray =
  "filter bool array"
    ~: TestList
      [ filterBoolArray test_valueArray ~?= [True, False],
        filterBoolArray [Boolean True, Boolean True] ~?= [True, True],
        filterBoolArray [] ~?= []
      ]

--- >>> runTestTT test_filterBoolArray
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}

test_filterStringArray :: Test
test_filterStringArray =
  "filter string array"
    ~: TestList
      [ filterStringArray test_valueArray ~?= ["hello", "world"],
        filterStringArray [String "Aakash", String "Yathu", String "552"]
          ~?= ["Aakash", "Yathu", "552"],
        filterStringArray
          []
          ~?= []
      ]

--- >>> runTestTT test_filterStringArray
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}

test_checkArrayLength :: Test
test_checkArrayLength =
  "filter number array"
    ~: TestList
      [ checkArrayLength test_valueArray ~?= False,
        checkArrayLength [String "Aakash", String "Yathu", String "552"]
          ~?= True,
        checkArrayLength [Integer 4]
          ~?= True,
        checkArrayLength
          []
          ~?= True
      ]

-- >>> runTestTT test_checkArrayLength
-- Counts {cases = 4, tried = 4, errors = 0, failures = 0}

test_boolArrayHelper :: Test
test_boolArrayHelper =
  "bool array helper"
    ~: TestList
      [ boolArrayHelper [] ~?= Nothing,
        boolArrayHelper [True, True] ~?= Just True,
        boolArrayHelper [False] ~?= Just False,
        boolArrayHelper [True, False, True] ~?= Nothing
      ]

--- >>> runTestTT test_boolArrayHelper
-- Counts {cases = 4, tried = 4, errors = 0, failures = 0}

------------------------- HSON to HSON Schema Tests -----------------------------

test_numberHelper :: Test
test_numberHelper =
  "number HSON helper"
    ~: TestList
      [ numberHelper (H [("minimum", Number 5.0), ("maximum", Number 8.5), ("exclusiveMinimum", Number 4.9), ("exclusiveMaximum", Number 8.6), ("multipleOf", Number 10.0), ("enum", Array [Number 1.0, Number 2.0])])
          ~?= Just
            ( Num $
                NP
                  { nMinimum = Just 5.0,
                    nMaximum = Just 8.5,
                    nExclusiveMinimum = Just 4.9,
                    nExclusiveMaximum = Just 8.6,
                    nMultipleOf = Just 10.0,
                    numberEnum = Just [1.0, 2.0]
                  }
            ),
        numberHelper (H [("minimum", Number 5.0), ("maximum", Number 8.5), ("exclusiveMinimum", Boolean True), ("exclusiveMaximum", Number 8.6), ("multipleOf", Number 10.0), ("enum", Array [Number 1.0, Number 2.0])])
          ~?= Nothing,
        numberHelper (H [("minimum", Number 5.0), ("maximum", Number 8.5), ("exclusiveMinimum", Number 4.9), ("exclusiveMaximum", Number 8.6), ("multipleOf", Number 10.0), ("enum", Array [Boolean True, Number 2.0])])
          ~?= Nothing,
        numberHelper (H [("minimum", Number 5.0), ("maximum", Number 8.5), ("exclusiveMinimum", Number 4.9), ("exclusiveMaximum", Number 8.6), ("multipleOf", Number 10.0), ("enum", Boolean True)])
          ~?= Nothing,
        numberHelper (H [("enum", Array [Number 5.0, Number 6.0])])
          ~?= Just
            ( Num $
                NP
                  { nMinimum = Nothing,
                    nMaximum = Nothing,
                    nExclusiveMinimum = Nothing,
                    nExclusiveMaximum = Nothing,
                    nMultipleOf = Nothing,
                    numberEnum = Just [5.0, 6.0]
                  }
            ),
        numberHelper (H [])
          ~?= Just
            ( Num $
                NP
                  { nMinimum = Nothing,
                    nMaximum = Nothing,
                    nExclusiveMinimum = Nothing,
                    nExclusiveMaximum = Nothing,
                    nMultipleOf = Nothing,
                    numberEnum = Nothing
                  }
            )
      ]

--- >>> runTestTT test_numberHelper
-- Counts {cases = 6, tried = 6, errors = 0, failures = 0}

test_intHelper :: Test
test_intHelper =
  "integer HSON helper"
    ~: TestList
      [ intHelper (H [("minimum", Integer 5), ("maximum", Integer 8), ("exclusiveMinimum", Integer 4), ("exclusiveMaximum", Integer 8), ("multipleOf", Integer 10), ("enum", Array [Integer 1, Integer 2])])
          ~?= Just
            ( Int $
                IP
                  { iMinimum = Just 5,
                    iMaximum = Just 8,
                    iExclusiveMinimum = Just 4,
                    iExclusiveMaximum = Just 8,
                    iMultipleOf = Just 10,
                    intEnum = Just [1, 2]
                  }
            ),
        intHelper (H [("minimum", Integer 5), ("maximum", Integer 8), ("exclusiveMinimum", Boolean True), ("exclusiveMaximum", Integer 8), ("multipleOf", Integer 10), ("enum", Array [Integer 1, Integer 2])])
          ~?= Nothing,
        intHelper (H [("minimum", Integer 5), ("maximum", Integer 8), ("exclusiveMinimum", Integer 4), ("exclusiveMaximum", Integer 8), ("multipleOf", Number 10.0), ("enum", Array [Integer 1, Number 2.0])])
          ~?= Nothing,
        intHelper (H [("minimum", Integer 5), ("maximum", Integer 8), ("exclusiveMinimum", Integer 4), ("exclusiveMaximum", Integer 8), ("multipleOf", Integer 10), ("enum", Boolean True)])
          ~?= Nothing,
        intHelper (H [("enum", Array [Integer 5, Integer 6])])
          ~?= Just
            ( Int $
                IP
                  { iMinimum = Nothing,
                    iMaximum = Nothing,
                    iExclusiveMinimum = Nothing,
                    iExclusiveMaximum = Nothing,
                    iMultipleOf = Nothing,
                    intEnum = Just [5, 6]
                  }
            ),
        intHelper (H [])
          ~?= Just
            ( Int $
                IP
                  { iMinimum = Nothing,
                    iMaximum = Nothing,
                    iExclusiveMinimum = Nothing,
                    iExclusiveMaximum = Nothing,
                    iMultipleOf = Nothing,
                    intEnum = Nothing
                  }
            )
      ]

--- >>> runTestTT test_intHelper
-- Counts {cases = 6, tried = 6, errors = 0, failures = 0}

test_stringHelper :: Test
test_stringHelper =
  "string HSON helper"
    ~: TestList
      [ stringHelper (H [("minLength", Integer 5), ("maxLength", Integer 8), ("pattern", String "a*"), ("enum", Array [String "aaaaa", String "aaaaaa"])])
          ~?= Just
            ( Str $
                SP
                  { minLength = Just 5,
                    maxLength = Just 8,
                    pattern = Just "a*",
                    stringEnum = Just ["aaaaa", "aaaaaa"]
                  }
            ),
        stringHelper (H [("minimum", Integer 5), ("maximum", Integer 8), ("pattern", Boolean True)])
          ~?= Nothing,
        stringHelper (H [("enum", Array [String "hi", Integer 1])])
          ~?= Nothing,
        stringHelper (H [("enum", Boolean True)])
          ~?= Nothing,
        stringHelper (H [("enum", Array [String "Aakash", String "Yathu"])])
          ~?= Just
            ( Str $
                SP
                  { minLength = Nothing,
                    maxLength = Nothing,
                    pattern = Nothing,
                    stringEnum = Just ["Aakash", "Yathu"]
                  }
            ),
        stringHelper (H [])
          ~?= Just
            ( Str $
                SP
                  { minLength = Nothing,
                    maxLength = Nothing,
                    pattern = Nothing,
                    stringEnum = Nothing
                  }
            )
      ]

--- >>> runTestTT test_stringHelper
-- Counts {cases = 6, tried = 6, errors = 0, failures = 0}

test_boolHelper :: Test
test_boolHelper =
  "boolean HSON helper"
    ~: TestList
      [ boolHelper (H [("enum", Array [Boolean True, Boolean True])])
          ~?= Just
            ( Bool $
                BP
                  { boolEnum = Just True
                  }
            ),
        boolHelper (H [("enum", Array [Boolean False, Boolean False])])
          ~?= Just
            ( Bool $
                BP
                  { boolEnum = Just False
                  }
            ),
        boolHelper (H [("enum", Array [Boolean True, Boolean False])])
          ~?= Just
            ( Bool $
                BP
                  { boolEnum = Nothing
                  }
            ),
        boolHelper (H [("enum", Array [Boolean False, String "hi", Integer 1])])
          ~?= Nothing,
        boolHelper (H [("enum", Boolean True)])
          ~?= Nothing,
        boolHelper (H [])
          ~?= Just
            ( Bool $
                BP
                  { boolEnum = Nothing
                  }
            )
      ]

--- >>> runTestTT test_boolHelper
-- Counts {cases = 6, tried = 6, errors = 0, failures = 0}

test_arrHelper :: Test
test_arrHelper =
  "array HSON helper"
    ~: TestList
      [ arrHelper (H [("minItems", Integer 5), ("maxItems", Integer 8), ("isUnique", Boolean True), ("items", Object (H [("type", String "integer")]))])
          ~?= Just
            ( Arr $
                AP
                  { minItems = Just 5,
                    maxItems = Just 8,
                    isUnique = True,
                    items =
                      Just $
                        Int $
                          IP
                            { iMinimum = Nothing,
                              iMaximum = Nothing,
                              iExclusiveMinimum = Nothing,
                              iExclusiveMaximum = Nothing,
                              iMultipleOf = Nothing,
                              intEnum = Nothing
                            }
                  }
            ),
        arrHelper (H [("minItems", Integer 5), ("maxItems", Number 8.0), ("isUnique", Boolean True)])
          ~?= Nothing,
        arrHelper (H [("items", Array [String "hi", Integer 1])])
          ~?= Nothing,
        arrHelper (H [("items", Boolean True)])
          ~?= Nothing,
        arrHelper (H [("items", Object (H [("types", String "integer")]))])
          ~?= Nothing,
        arrHelper (H [("items", Object (H [("type", String "yathu")]))])
          ~?= Nothing,
        arrHelper
          (H [])
          ~?= Just
            ( Arr $
                AP
                  { minItems = Nothing,
                    maxItems = Nothing,
                    isUnique = False,
                    items = Nothing
                  }
            )
      ]

--- >>> runTestTT test_arrHelper

test_objHelper :: Test
test_objHelper =
  "string HSON helper"
    ~: TestList
      [ objHelper
          ( H
              [ ("minProperties", Integer 5),
                ("maxProperties", Integer 8),
                ("required", Array [String "prop1", String "prop2"]),
                ( "properties",
                  Object
                    ( H
                        [ ("prop1", Object (H [("type", String "integer"), ("minimum", Integer 5)])),
                          ("prop2", Object (H [("type", String "integer"), ("maximum", Integer 8)]))
                        ]
                    )
                )
              ]
          )
          ~?= Just
            ( Obj $
                OP
                  { minProperties = Just 5,
                    maxProperties = Just 8,
                    required = ["prop1", "prop2"],
                    properties =
                      [ ( "prop1",
                          Int $
                            IP
                              { iMinimum = Just 5,
                                iMaximum = Nothing,
                                iExclusiveMinimum = Nothing,
                                iExclusiveMaximum = Nothing,
                                iMultipleOf = Nothing,
                                intEnum = Nothing
                              }
                        ),
                        ( "prop2",
                          Int $
                            IP
                              { iMinimum = Nothing,
                                iMaximum = Just 8,
                                iExclusiveMinimum = Nothing,
                                iExclusiveMaximum = Nothing,
                                iMultipleOf = Nothing,
                                intEnum = Nothing
                              }
                        )
                      ]
                  }
            ),
        objHelper (H [("minProperties", Integer 5), ("maxProperties", Number 8.0), ("required", Array [String "a"])])
          ~?= Nothing,
        objHelper (H [("required", Array [String "hi", Integer 1])])
          ~?= Nothing,
        objHelper (H [("required", Boolean True)])
          ~?= Nothing,
        objHelper (H [("items", Object (H [("types", String "integer")]))])
          ~?= Just
            ( Obj $
                OP
                  { maxProperties = Nothing,
                    minProperties = Nothing,
                    required = [],
                    properties = []
                  }
            ),
        objHelper
          (H [])
          ~?= Just
            ( Obj $
                OP
                  { maxProperties = Nothing,
                    minProperties = Nothing,
                    required = [],
                    properties = []
                  }
            )
      ]

--- >>> runTestTT test_objHelper
-- Counts {cases = 6, tried = 6, errors = 0, failures = 0}

-------------------------- Create HSON Schema Tests ------------------------------------

tCreateHSONSchema :: Test
tCreateHSONSchema =
  "create HSON schema"
    ~: TestList
      [ "address" ~: p "test/json-schema/schema/address-schema.json" address,
        "card" ~: p "test/json-schema/schema/card-schema.json" card,
        "coordinate" ~: p "test/json-schema/schema/coordinate-schema.json" coordinate
      ]
  where
    p fn hschema = do
      z <- parseJSON fn
      case z of
        (Left _) -> assert False
        (Right ast') -> do
          assert (hschema == hsonToHSONSchema ast')

-- >>> runTestTT tCreateHSONSchema
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}

test_toHSONSchema :: IO Counts
test_toHSONSchema =
  runTestTT $
    TestList
      [ test_matchInt,
        test_matchNumber,
        test_matchBool,
        test_matchString,
        test_filterIntArray,
        test_filterNumberArray,
        test_filterBoolArray,
        test_filterStringArray,
        test_checkArrayLength,
        test_boolArrayHelper,
        test_numberHelper,
        test_intHelper,
        test_stringHelper,
        test_boolHelper,
        test_arrHelper,
        test_objHelper,
        tCreateHSONSchema
      ]

-- >>> test_toHSONSchema
