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
  | Bool
  | Arr ArrProperties
  | Obj ObjProperties
  | Nul
  deriving (Show, Eq)

-- newtype Schema = { validate :: HSON -> Bool } 


-- Minimum, Exclusive Minimum, Maximum, Exclusive Maximum
data IntProperties
  = IP
      (Maybe Int)
      (Maybe Int)
      (Maybe Int)
      (Maybe Int)
  deriving (Show, Eq)

-- MinLength, MaxLength, Pattern
data StrProperties
  = SP
      (Maybe Int)
      (Maybe Int)
      (Maybe String)
  deriving (Show, Eq)

-- MaxItems, MinItems, isUnique, Items
data ArrProperties
  = AP
      (Maybe Int)
      (Maybe Int)
      Bool
      (Maybe HSONSchema)
  deriving (Show, Eq)

-- MinProperties, MaxProperties, Required
data ObjProperties
  = OP
      (Maybe Int)
      (Maybe Int)
      [String]
      (Map Attribute HSONSchema)
  deriving (Show, Eq)

address :: HSONSchema
address =
  Obj
    ( OP
        Nothing
        Nothing
        ["locality", "zip-code", "country-name"]
        ( Map.fromList
            [ ("post-office-box", Str $ SP Nothing Nothing Nothing),
              ("extended-address", Str $ SP Nothing Nothing Nothing),
              ("street-address", Str $ SP Nothing Nothing Nothing),
              ("locality", Str $ SP Nothing Nothing Nothing),
              ("zip-code", Int $ IP Nothing Nothing Nothing Nothing)
            ]
        )
    )

card :: HSONSchema
card =
  Obj
    ( OP
        Nothing
        Nothing
        ["familyName", "givenName"]
        ( Map.fromList
            [ ("fn", Str $ SP Nothing Nothing Nothing),
              ("family-name", Str $ SP Nothing Nothing Nothing),
              ("given-name", Str $ SP Nothing Nothing Nothing),
              ("additional-name", Arr $ AP Nothing Nothing True (Just (Str $ SP Nothing Nothing Nothing))),
              ("nickname", Str $ SP Nothing Nothing Nothing),
              ( "email",
                Obj $
                  OP
                    Nothing
                    Nothing
                    []
                    ( Map.fromList
                        [ ("type", Str $ SP Nothing Nothing Nothing),
                          ("value", Str $ SP Nothing Nothing Nothing)
                        ]
                    )
              ),
              ( "org",
                Obj $
                  OP
                    Nothing
                    Nothing
                    []
                    ( Map.fromList
                        [ ("organizationName", Str $ SP Nothing Nothing Nothing),
                          ("organizationUnit", Str $ SP Nothing Nothing Nothing)
                        ]
                    )
              )
            ]
        )
    )

coordinate :: HSONSchema
coordinate =
  Obj $
    OP
      Nothing
      Nothing
      ["latitude", "longitude"]
      ( Map.fromList
          [ ("latitude", Num $ IP (Just (-90)) Nothing (Just 90) Nothing),
            ("longitude", Num $ IP (Just (-180)) Nothing (Just 80) Nothing)
          ]
      )

type Attribute = String

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
