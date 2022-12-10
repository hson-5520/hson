{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module HSONSchema (HSONSchema (Str, Int, Num, Bool, Arr, Obj, Nul), IntProperties (IP), NumProperties (NP), nMinimum, nMaximum, nExclusiveMinimum, nExclusiveMaximum, nMultipleOf, numberEnum, StrProperties (SP), ArrProperties (AP), minItems, maxItems, isUnique, minProperties, maxProperties, required, properties, ObjProperties (OP), items, boolEnum, BoolProperties (BP), minLength, maxLength, pattern, stringEnum, iMaximum, iMinimum, exclusiveMinimum, exclusiveMaximum, multipleOf, intEnum, address, card, coordinate, ArrProperties, ObjProperties) where

import Data.Map
import Data.Map qualified as Map
import HSON (HSON, Key, Value (Array, Boolean, Null, Number, Object, String))
import Parser qualified as P

------------------------- Defining HSON Schema  --------------------------------

data HSONSchema
  = Str StrProperties
  | Int IntProperties
  | Num NumProperties
  | Bool BoolProperties
  | Arr ArrProperties
  | Obj ObjProperties
  | Nul
  deriving (Show, Eq)

data IntProperties = IP
  { iMinimum :: Maybe Int,
    iMaximum :: Maybe Int,
    exclusiveMinimum :: Maybe Int,
    exclusiveMaximum :: Maybe Int,
    multipleOf :: Maybe Int,
    intEnum :: Maybe [Int]
  }
  deriving (Show, Eq)

data NumProperties = NP
  { nMinimum :: Maybe Int,
    nMaximum :: Maybe Int,
    nExclusiveMinimum :: Maybe Int,
    nExclusiveMaximum :: Maybe Int,
    nMultipleOf :: Maybe Double,
    numberEnum :: Maybe [Double]
  }
  deriving (Show, Eq)

data StrProperties = SP
  { minLength :: Maybe Int,
    maxLength :: Maybe Int,
    pattern :: Maybe String,
    stringEnum :: Maybe [String]
  }
  deriving (Show, Eq)

data BoolProperties = BP {boolEnum :: Maybe Bool}
  deriving (Show, Eq)

data ArrProperties = AP
  { minItems :: Maybe Int,
    maxItems :: Maybe Int,
    isUnique :: Bool,
    items :: Maybe HSONSchema
  }
  deriving (Show, Eq)

data ObjProperties = OP
  { minProperties :: Maybe Int,
    maxProperties :: Maybe Int,
    required :: [String],
    properties :: [(Key, HSONSchema)]
  }
  deriving (Show, Eq)

address :: HSONSchema
address =
  Obj $
    OP
      { minProperties = Nothing,
        maxProperties = Nothing,
        required = ["locality", "zip-code", "country-name"],
        properties =
          [ ("post-office-box", Str $ SP {minLength = Nothing, maxLength = Nothing, pattern = Nothing, stringEnum = Nothing}),
            ("extended-address", Str $ SP {minLength = Nothing, maxLength = Nothing, pattern = Nothing, stringEnum = Nothing}),
            ("street-address", Str $ SP {minLength = Nothing, maxLength = Nothing, pattern = Nothing, stringEnum = Nothing}),
            ("locality", Str $ SP {minLength = Nothing, maxLength = Nothing, pattern = Nothing, stringEnum = Nothing}),
            ("zip-code", Int $ IP {iMinimum = Nothing, iMaximum = Nothing, exclusiveMaximum = Nothing, exclusiveMinimum = Nothing, multipleOf = Nothing, intEnum = Nothing})
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
          [ ("fn", Str $ SP {minLength = Nothing, maxLength = Nothing, pattern = Nothing, stringEnum = Nothing}),
            ("family-name", Str $ SP {minLength = Nothing, maxLength = Nothing, pattern = Nothing, stringEnum = Nothing}),
            ("given-name", Str $ SP {minLength = Nothing, maxLength = Nothing, pattern = Nothing, stringEnum = Nothing}),
            ("additional-name", Arr $ AP {minItems = Nothing, maxItems = Nothing, isUnique = False, items = Nothing}),
            ("nickname", Str $ SP {minLength = Nothing, maxLength = Nothing, pattern = Nothing, stringEnum = Nothing}),
            ( "email",
              Obj $
                OP
                  { minProperties = Nothing,
                    maxProperties = Nothing,
                    required = [],
                    properties =
                      [ ("type", Str $ SP {minLength = Nothing, maxLength = Nothing, pattern = Nothing, stringEnum = Nothing}),
                        ("value", Str $ SP {minLength = Nothing, maxLength = Nothing, pattern = Nothing, stringEnum = Nothing})
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
                      [ ("organizationName", Str $ SP {minLength = Nothing, maxLength = Nothing, pattern = Nothing, stringEnum = Nothing}),
                        ("organizationUnit", Str $ SP {minLength = Nothing, maxLength = Nothing, pattern = Nothing, stringEnum = Nothing})
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
          [ ("latitude", Num $ NP {nMinimum = Just (-90), nMaximum = Just 90, nExclusiveMinimum = Nothing, nExclusiveMaximum = Nothing, nMultipleOf = Nothing, numberEnum = Nothing}),
            ("longitude", Num $ NP {nMinimum = Just (-180), nMaximum = Just 180, nExclusiveMinimum = Nothing, nExclusiveMaximum = Nothing, nMultipleOf = Nothing, numberEnum = Nothing})
          ]
      }