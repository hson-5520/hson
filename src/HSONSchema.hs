module HSONSchema where

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
