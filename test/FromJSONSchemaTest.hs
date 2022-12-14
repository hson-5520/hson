module FromJSONSchemaTest where

import Control.Applicative
import Data.Bits (Bits (xor))
import Data.Bool (bool)
import Data.Map qualified as Map
import Data.Maybe
import FromJSON
import FromJSONSchema
import HSON
  ( HSON (H),
    Key,
    Value (Array, Boolean, Integer, Null, Number, Object, String),
  )
import HSONSchema
  ( ArrProperties (..),
    BoolProperties (..),
    HSONSchema (..),
    IntProperties (..),
    NumProperties (..),
    ObjProperties (..),
    StrProperties (..),
  )
import Parser qualified as P
import Test.HUnit (Counts, Test (TestList), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck
import ToJSON

------------------------ FromJSONSchema Helpers' Tests  ------------------------

test_matchInt :: Test
test_matchInt =
  "match int"
    ~: TestList
      [ matchInt
          ""
          "test"
          (Map.fromList [("test", Integer 2)])
          ~?= Right (Just 2),
        matchInt
          ""
          "test"
          Map.empty
          ~?= Right Nothing,
        matchInt
          ""
          "test"
          (Map.fromList [("randomKey", Boolean True)])
          ~?= Right Nothing,
        matchInt
          ""
          "test"
          (Map.fromList [("randomKey", Boolean True), ("test", Integer 2)])
          ~?= Right (Just 2),
        matchInt
          "error"
          "test"
          (Map.fromList [("randomKey", Boolean True), ("test", String "abcVal")])
          ~?= Left "error"
      ]

-- >>> runTestTT test_matchInt

test_matchNumber :: Test
test_matchNumber =
  "match number"
    ~: TestList
      [ matchNumber
          ""
          "test"
          (Map.fromList [("test", Number 2.0)])
          ~?= Right (Just 2.0),
        matchNumber
          ""
          "test"
          Map.empty
          ~?= Right Nothing,
        matchNumber
          ""
          "test"
          (Map.fromList [("randomKey", String "True")])
          ~?= Right Nothing,
        matchNumber
          ""
          "test"
          (Map.fromList [("randomKey", Boolean True), ("test", Number 5.0)])
          ~?= Right (Just 5.0),
        matchNumber
          ""
          "test"
          (Map.fromList [("randomKey", Boolean True), ("test", Integer 4)])
          ~?= Right (Just 4.0)
      ]

-- >>> runTestTT test_matchNumber

test_matchBool :: Test
test_matchBool =
  "match bool"
    ~: TestList
      [ matchBool
          ""
          "boolKey"
          (Map.fromList [("boolKey", Boolean True)])
          ~?= Right (Just True),
        matchBool
          ""
          "boolKey"
          Map.empty
          ~?= Right Nothing,
        matchBool
          ""
          "boolKey"
          (Map.fromList [("randomKey", String "True")])
          ~?= Right Nothing,
        matchBool
          ""
          "boolKey"
          (Map.fromList [("randomKey", Boolean True), ("boolKey", Boolean False)])
          ~?= Right (Just False),
        matchBool
          "error"
          "boolKey"
          (Map.fromList [("randomKey", Boolean True), ("boolKey", Integer 4)])
          ~?= Left "error"
      ]

-- >>> runTestTT test_matchBool

test_matchString :: Test
test_matchString =
  "match string"
    ~: TestList
      [ matchString
          ""
          "test"
          (Map.fromList [("test", String "hello world")])
          ~?= Right (Just "hello world"),
        matchString
          ""
          "test"
          Map.empty
          ~?= Right Nothing,
        matchString
          ""
          "test"
          (Map.fromList [("randomKey", String "True")])
          ~?= Right Nothing,
        matchString
          ""
          "test"
          (Map.fromList [("randomKey", Boolean True), ("test", String "hello")])
          ~?= Right (Just "hello"),
        matchString
          "error"
          "test"
          (Map.fromList [("randomKey", Boolean True), ("test", Integer 4)])
          ~?= Left "error"
      ]

-- >>> runTestTT test_matchString

test_valueArray :: [Value]
test_valueArray =
  [ Integer 1,
    Number 2.0,
    Boolean True,
    String "hello",
    Integer 3,
    Number 4.0,
    Boolean False,
    String "world"
  ]

test_filterIntArray :: Test
test_filterIntArray =
  "filter int array"
    ~: TestList
      [ filterIntArray test_valueArray ~?= [1, 3],
        filterIntArray [Integer 5, Integer 7, Integer 9] ~?= [5, 7, 9],
        filterIntArray [] ~?= []
      ]

-- >>> runTestTT test_filterIntArray

test_filterNumberArray :: Test
test_filterNumberArray =
  "filter number array"
    ~: TestList
      [ filterNumberArray test_valueArray ~?= [2.0, 4.0],
        filterNumberArray [Number 5.1, Number 7.2, Number 9.3]
          ~?= [5.1, 7.2, 9.3],
        filterNumberArray [] ~?= []
      ]

--- >>> runTestTT test_filterNumberArray

test_filterBoolArray :: Test
test_filterBoolArray =
  "filter bool array"
    ~: TestList
      [ filterBoolArray test_valueArray ~?= [True, False],
        filterBoolArray [Boolean True, Boolean True] ~?= [True, True],
        filterBoolArray [] ~?= []
      ]

--- >>> runTestTT test_filterBoolArray

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

test_isArrayConsistent :: Test
test_isArrayConsistent =
  "filter number array"
    ~: TestList
      [ isArrayConsistent test_valueArray ~?= False,
        isArrayConsistent [String "Aakash", String "Yathu", String "552"]
          ~?= True,
        isArrayConsistent [Integer 4]
          ~?= True,
        isArrayConsistent
          []
          ~?= True
      ]

-- >>> runTestTT isArrayConsistent

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

------------------------- HSON to HSON Schema Tests ----------------------------

test_numberHelper :: Test
test_numberHelper =
  "number HSON helper"
    ~: TestList
      [ numberHelper
          ( H
              [ ("minimum", Number 5.0),
                ("maximum", Number 8.5),
                ("exclusiveMinimum", Number 4.9),
                ("exclusiveMaximum", Number 8.6),
                ("multipleOf", Number 10.0),
                ("enum", Array [Number 1.0, Number 2.0])
              ]
          )
          ~?= Right
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
        numberHelper
          ( H
              [ ("minimum", Number 5.0),
                ("maximum", Number 8.5),
                ("exclusiveMinimum", Boolean True),
                ("exclusiveMaximum", Number 8.6),
                ("multipleOf", Number 10.0),
                ("enum", Array [Number 1.0, Number 2.0])
              ]
          )
          ~?= Left "exclusiveMinimum is not a number",
        numberHelper
          ( H
              [ ("minimum", Number 5.0),
                ("maximum", Number 8.5),
                ("exclusiveMinimum", Number 4.9),
                ("exclusiveMaximum", Number 8.6),
                ("multipleOf", Number 10.0),
                ("enum", Array [Boolean True, Number 2.0])
              ]
          )
          ~?= Left "enum has elements of different types",
        numberHelper
          ( H
              [ ("minimum", Number 5.0),
                ("maximum", Number 8.5),
                ("exclusiveMinimum", Number 4.9),
                ("exclusiveMaximum", Number 8.6),
                ("multipleOf", Number 10.0),
                ("enum", Boolean True)
              ]
          )
          ~?= Left "enum is not an array",
        numberHelper (H [("enum", Array [Number 5.0, Number 6.0])])
          ~?= Right
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
          ~?= Right
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

test_intHelper :: Test
test_intHelper =
  "integer HSON helper"
    ~: TestList
      [ intHelper
          ( H
              [ ("minimum", Integer 5),
                ("maximum", Integer 8),
                ("exclusiveMinimum", Integer 4),
                ("exclusiveMaximum", Integer 8),
                ("multipleOf", Integer 10),
                ("enum", Array [Integer 1, Integer 2])
              ]
          )
          ~?= Right
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
        intHelper
          ( H
              [ ("minimum", Integer 5),
                ("maximum", Integer 8),
                ("exclusiveMinimum", Boolean True),
                ("exclusiveMaximum", Integer 8),
                ("multipleOf", Integer 10),
                ("enum", Array [Integer 1, Integer 2])
              ]
          )
          ~?= Left "exclusiveMinimum is not an integer",
        intHelper
          ( H
              [ ("minimum", Integer 5),
                ("maximum", Integer 8),
                ("exclusiveMinimum", Integer 4),
                ("exclusiveMaximum", Integer 8),
                ("multipleOf", Number 10.0),
                ("enum", Array [Integer 1, Number 2.0])
              ]
          )
          ~?= Left "multipleOf is not an integer",
        intHelper
          ( H
              [ ("minimum", Integer 5),
                ("maximum", Integer 8),
                ("exclusiveMinimum", Integer 4),
                ("exclusiveMaximum", Integer 8),
                ("multipleOf", Integer 10),
                ("enum", Boolean True)
              ]
          )
          ~?= Left "enum is not an array",
        intHelper (H [("enum", Array [Integer 5, Integer 6])])
          ~?= Right
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
          ~?= Right
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

test_stringHelper :: Test
test_stringHelper =
  "string HSON helper"
    ~: TestList
      [ stringHelper
          ( H
              [ ("minLength", Integer 5),
                ("maxLength", Integer 8),
                ("pattern", String "a*"),
                ("enum", Array [String "aaaaa", String "aaaaaa"])
              ]
          )
          ~?= Right
            ( Str $
                SP
                  { minLength = Just 5,
                    maxLength = Just 8,
                    pattern = Just "a*",
                    stringEnum = Just ["aaaaa", "aaaaaa"]
                  }
            ),
        stringHelper
          ( H
              [ ("minimum", Integer 5),
                ("maximum", Integer 8),
                ("pattern", Boolean True)
              ]
          )
          ~?= Left "pattern is not a string",
        stringHelper (H [("enum", Array [String "hi", Integer 1])])
          ~?= Left "enum has elements of different types",
        stringHelper (H [("enum", Boolean True)])
          ~?= Left "enum is not an array",
        stringHelper (H [("enum", Array [String "Aakash", String "Yathu"])])
          ~?= Right
            ( Str $
                SP
                  { minLength = Nothing,
                    maxLength = Nothing,
                    pattern = Nothing,
                    stringEnum = Just ["Aakash", "Yathu"]
                  }
            ),
        stringHelper (H [])
          ~?= Right
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

test_boolHelper :: Test
test_boolHelper =
  "boolean HSON helper"
    ~: TestList
      [ boolHelper (H [("enum", Array [Boolean True, Boolean True])])
          ~?= Right
            ( Bool $
                BP
                  { boolEnum = Just True
                  }
            ),
        boolHelper (H [("enum", Array [Boolean False, Boolean False])])
          ~?= Right
            ( Bool $
                BP
                  { boolEnum = Just False
                  }
            ),
        boolHelper (H [("enum", Array [Boolean True, Boolean False])])
          ~?= Right
            ( Bool $
                BP
                  { boolEnum = Nothing
                  }
            ),
        boolHelper (H [("enum", Array [Boolean False, String "hi", Integer 1])])
          ~?= Left "enum has elements of different types",
        boolHelper (H [("enum", Boolean True)])
          ~?= Left "enum is not an array",
        boolHelper (H [])
          ~?= Right
            ( Bool $
                BP
                  { boolEnum = Nothing
                  }
            )
      ]

--- >>> runTestTT test_boolHelper

test_arrHelper :: Test
test_arrHelper =
  "array HSON helper"
    ~: TestList
      [ arrHelper
          ( H
              [ ("minItems", Integer 5),
                ("maxItems", Integer 8),
                ("isUnique", Boolean True),
                ("items", Object (H [("type", String "integer")]))
              ]
          )
          ~?= Right
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
        arrHelper
          ( H
              [ ("minItems", Integer 5),
                ("maxItems", Number 8.0),
                ("isUnique", Boolean True)
              ]
          )
          ~?= Left "maxItems is not an integer",
        arrHelper (H [("items", Array [String "hi", Integer 1])])
          ~?= Left "items is not an object",
        arrHelper (H [("items", Boolean True)])
          ~?= Left "items is not an object",
        arrHelper (H [("items", Object (H [("types", String "integer")]))])
          ~?= Left "",
        arrHelper (H [("items", Object (H [("type", String "yathu")]))])
          ~?= Left "type is not a valid string",
        arrHelper
          (H [])
          ~?= Right
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
                        [ ( "prop1",
                            Object
                              ( H
                                  [ ("type", String "integer"),
                                    ("minimum", Integer 5)
                                  ]
                              )
                          ),
                          ( "prop2",
                            Object
                              ( H
                                  [ ("type", String "integer"),
                                    ("maximum", Integer 8)
                                  ]
                              )
                          )
                        ]
                    )
                )
              ]
          )
          ~?= Right
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
        objHelper
          ( H
              [ ("minProperties", Integer 5),
                ("maxProperties", Number 8.0),
                ("required", Array [String "a"])
              ]
          )
          ~?= Left "maxProperties is not an integer",
        objHelper (H [("required", Array [String "hi", Integer 1])])
          ~?= Left "required contains non-string element(s)",
        objHelper (H [("required", Boolean True)])
          ~?= Left "required is not an array",
        objHelper (H [("items", Object (H [("types", String "integer")]))])
          ~?= Right
            ( Obj $
                OP
                  { maxProperties = Nothing,
                    minProperties = Nothing,
                    required = [],
                    properties = []
                  }
            ),
        objHelper
          ( H
              [ ( "properties",
                  Object
                    ( H
                        [ ( "bob",
                            Object
                              ( H
                                  [ ("type", String "object"),
                                    ( "properties",
                                      Object
                                        ( H
                                            [ ("type", String "notavalue")
                                            ]
                                        )
                                    )
                                  ]
                              )
                          )
                        ]
                    )
                )
              ]
          )
          ~?= Left "bob: attributes is not an object",
        objHelper
          ( H
              [ ( "properties",
                  Object
                    ( H
                        [ ( "bob",
                            Object
                              ( H
                                  [ ("type", String "object"),
                                    ( "properties",
                                      Object
                                        ( H
                                            [ ( "jim",
                                                Object
                                                  ( H
                                                      [ ( "type",
                                                          String "notavalue"
                                                        )
                                                      ]
                                                  )
                                              )
                                            ]
                                        )
                                    )
                                  ]
                              )
                          )
                        ]
                    )
                )
              ]
          )
          ~?= Left "bob: jim: type is not a valid string",
        objHelper
          ( H
              [ ( "properties",
                  Object
                    ( H
                        [ ("bob", Object (H [("type", String "notavalue")]))
                        ]
                    )
                )
              ]
          )
          ~?= Left "bob: type is not a valid string",
        objHelper
          (H [])
          ~?= Right
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

----------------------------- HSONSchema ---------------------------------------

-- | HSONSchema representation of a JSON Schema file
address :: HSONSchema
address =
  Obj $
    OP
      { minProperties = Nothing,
        maxProperties = Nothing,
        required = ["locality", "zip-code"],
        properties =
          [ ( "post-office-box",
              Str $
                SP
                  { minLength = Nothing,
                    maxLength = Nothing,
                    pattern = Nothing,
                    stringEnum = Nothing
                  }
            ),
            ( "extended-address",
              Str $
                SP
                  { minLength = Nothing,
                    maxLength = Nothing,
                    pattern = Nothing,
                    stringEnum = Nothing
                  }
            ),
            ( "street-address",
              Str $
                SP
                  { minLength = Nothing,
                    maxLength = Nothing,
                    pattern = Nothing,
                    stringEnum = Nothing
                  }
            ),
            ( "locality",
              Str $
                SP
                  { minLength = Nothing,
                    maxLength = Nothing,
                    pattern = Nothing,
                    stringEnum = Nothing
                  }
            ),
            ( "zip-code",
              Int $
                IP
                  { iMinimum = Nothing,
                    iMaximum = Nothing,
                    iExclusiveMaximum = Nothing,
                    iExclusiveMinimum = Nothing,
                    iMultipleOf = Nothing,
                    intEnum = Nothing
                  }
            )
          ]
      }

card :: HSONSchema
card =
  Obj $
    OP
      { minProperties = Nothing,
        maxProperties = Nothing,
        required = ["familyName", "givenName"],
        properties =
          [ ( "fn",
              Str $
                SP
                  { minLength = Nothing,
                    maxLength = Nothing,
                    pattern = Nothing,
                    stringEnum = Nothing
                  }
            ),
            ( "familyName",
              Str $
                SP
                  { minLength = Nothing,
                    maxLength = Nothing,
                    pattern = Nothing,
                    stringEnum = Nothing
                  }
            ),
            ("givenName", Nul),
            ( "additionalName",
              Arr $
                AP
                  { minItems = Nothing,
                    maxItems = Nothing,
                    isUnique = False,
                    items =
                      Just $
                        Str $
                          SP
                            { minLength = Nothing,
                              maxLength = Nothing,
                              pattern = Nothing,
                              stringEnum = Nothing
                            }
                  }
            ),
            ( "nickname",
              Str $
                SP
                  { minLength = Nothing,
                    maxLength = Nothing,
                    pattern = Nothing,
                    stringEnum = Nothing
                  }
            ),
            ( "email",
              Obj $
                OP
                  { minProperties = Nothing,
                    maxProperties = Nothing,
                    required = [],
                    properties =
                      [ ( "type",
                          Str $
                            SP
                              { minLength = Nothing,
                                maxLength = Nothing,
                                pattern = Nothing,
                                stringEnum = Nothing
                              }
                        ),
                        ( "value",
                          Str $
                            SP
                              { minLength = Nothing,
                                maxLength = Nothing,
                                pattern = Nothing,
                                stringEnum = Nothing
                              }
                        )
                      ]
                  }
            ),
            ( "org",
              Obj $
                OP
                  { minProperties = Nothing,
                    maxProperties = Nothing,
                    required = [],
                    properties =
                      [ ( "organizationName",
                          Str $
                            SP
                              { minLength = Nothing,
                                maxLength = Nothing,
                                pattern = Nothing,
                                stringEnum = Nothing
                              }
                        ),
                        ( "organizationUnit",
                          Str $
                            SP
                              { minLength = Nothing,
                                maxLength = Nothing,
                                pattern = Nothing,
                                stringEnum = Nothing
                              }
                        )
                      ]
                  }
            )
          ]
      }

coordinate :: HSONSchema
coordinate =
  Obj $
    OP
      { minProperties = Nothing,
        maxProperties = Nothing,
        required = ["latitude", "longitude"],
        properties =
          [ ( "latitude",
              Num $
                NP
                  { nMinimum = Just (-90),
                    nMaximum = Just 90,
                    nExclusiveMinimum = Nothing,
                    nExclusiveMaximum = Nothing,
                    nMultipleOf = Nothing,
                    numberEnum = Nothing
                  }
            ),
            ( "longitude",
              Num $
                NP
                  { nMinimum = Just (-180),
                    nMaximum = Just 180,
                    nExclusiveMinimum = Nothing,
                    nExclusiveMaximum = Nothing,
                    nMultipleOf = Nothing,
                    numberEnum = Nothing
                  }
            )
          ]
      }

------------------------ FromJSONSchema Schema Tests ---------------------------

tCreateHSONSchema :: Test
tCreateHSONSchema =
  "create HSON schema"
    ~: TestList
      [ "address" ~: p "test/json-schema/schema/address-schema.json" address,
        "card" ~: p "test/json-schema/schema/card-schema.json" card,
        "coordinate"
          ~: p "test/json-schema/schema/coordinate-schema.json" coordinate
      ]
  where
    p fn hschema = do
      hsonSchema <- fromJSONSchema fn
      case hsonSchema of
        Nothing -> assert False
        Just hs -> assert (hschema == hs)

-- >>> runTestTT tCreateHSONSchema

------------------------------- All Tests --------------------------------------

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
        test_isArrayConsistent,
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

--------------------------------------------------------------------------------
