module HSONSchema (HSONSchema (Str, Int, Num, Bool, Arr, Obj, Nul), IntProperties (IP), NumProperties (NP), nMinimum, nMaximum, nExclusiveMinimum, nExclusiveMaximum, nMultipleOf, numberEnum, StrProperties (SP), ArrProperties (AP), minItems, maxItems, isUnique, minProperties, maxProperties, required, properties, ObjProperties (OP), items, boolEnum, BoolProperties (BP), minLength, maxLength, pattern, stringEnum, iMaximum, iMinimum, iExclusiveMinimum, iExclusiveMaximum, iMultipleOf, intEnum, address, card, coordinate) where

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
    iExclusiveMinimum :: Maybe Int,
    iExclusiveMaximum :: Maybe Int,
    iMultipleOf :: Maybe Int,
    intEnum :: Maybe [Int]
  }
  deriving (Show, Eq)

data NumProperties = NP
  { nMinimum :: Maybe Double,
    nMaximum :: Maybe Double,
    nExclusiveMinimum :: Maybe Double,
    nExclusiveMaximum :: Maybe Double,
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
    properties :: [(Key, HSONSchema)] -- fromList hson lookup key and then perform relvant on that key
  }
  deriving (Show, Eq)

address :: Maybe HSONSchema
address =
  Just $
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
              ("zip-code", Int $ IP {iMinimum = Nothing, iMaximum = Nothing, iExclusiveMaximum = Nothing, iExclusiveMinimum = Nothing, iMultipleOf = Nothing, intEnum = Nothing})
            ]
        }

card :: Maybe HSONSchema
card =
  Just $
    Obj $
      OP
        { minProperties = Nothing,
          maxProperties = Nothing,
          required = ["familyName", "givenName"],
          properties =
            [ ("fn", Str $ SP {minLength = Nothing, maxLength = Nothing, pattern = Nothing, stringEnum = Nothing}),
              ("familyName", Str $ SP {minLength = Nothing, maxLength = Nothing, pattern = Nothing, stringEnum = Nothing}),
              ("givenName", Nul),
              ( "additionalName",
                Arr $
                  AP
                    { minItems = Nothing,
                      maxItems = Nothing,
                      isUnique = False,
                      items =
                        Just $ Str $ SP {minLength = Nothing, maxLength = Nothing, pattern = Nothing, stringEnum = Nothing}
                    }
              ),
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

coordinate :: Maybe HSONSchema
coordinate =
  Just $
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
