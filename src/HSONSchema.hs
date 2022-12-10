{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
module HSONSchema (HSONSchema, address, card, coordinate, hsonToHSONSchema, IntProperties, StrProperties, ArrProperties, ObjProperties) where

import Data.Map
import Data.Map qualified as Map
import HSON (HSON, Key, Value (Array, Boolean, Null, Number, Object, String))
import Parser qualified as P

------------------------- Defining HSON Schema  --------------------------------

data HSONSchema
  = Str StrProperties
  | Int IntProperties
  | Num IntProperties
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
    intEnum :: Maybe [Int]
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
            ("zip-code", Int $ IP {iMinimum = Nothing, iMaximum = Nothing, exclusiveMaximum = Nothing, exclusiveMinimum = Nothing, intEnum = Nothing})
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
          [ ("latitude", Num $ IP {iMinimum = (Just (-90)), iMaximum = (Just 90), exclusiveMinimum = Nothing, exclusiveMaximum = Nothing, intEnum = Nothing}),
            ("longitude", Num $ IP {iMinimum = (Just (-180)), iMaximum = (Just 180), exclusiveMinimum = Nothing, exclusiveMaximum = Nothing, intEnum = Nothing})
          ]
      }

------------------------- HSON to HSON Schema  ---------------------------------

-- | converts a HSON object representing a number property to an HSON Schema num property
-- | Num (IP Minimum ExclusiveMinimum Maximum ExclusiveMaximum)
numberHelper :: HSON -> HSONSchema
numberHelper x = undefined

-- | converts a HSON object representing an int property to an HSON Schema int property
-- | Int (IP Minimum ExclusiveMinimum Maximum ExclusiveMaximum)
intHelper :: HSON -> HSONSchema
intHelper x = undefined

-- | converts a HSON object representing a string property to an HSON Schema str property
-- | Str (SP MinLength MaxLength Pattern)
stringHelper :: HSON -> HSONSchema
stringHelper x = undefined

-- | converts a HSON object representing a booleaan property to an HSON Schema bool property
-- | Bool (BP)
boolHelper :: HSON -> HSONSchema
boolHelper x = undefined

-- | converts a HSON object representing an array property to an HSON Schema arr property
-- | Arr (AP MaxItems MinItems isUnique Items)
arrHelper :: HSON -> HSONSchema
arrHelper x = undefined

-- | converts a HSON object representing an object property to an HSON Schema obj property
-- | Obj (OP MinProperties MaxProperties Required)
objHelper :: HSON -> HSONSchema
objHelper x = undefined

-- | converts an entire HSON object to it's corresponding HSONSchema object
hsonToHSONSchema :: HSON -> HSONSchema
hsonToHSONSchema hson = objHelper hson
