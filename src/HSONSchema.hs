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

-- MinProperties, Max Properties, Required,
data ObjProperties
  = OP
      (Maybe Int)
      (Maybe Int)
      [String]
      (Map Attribute HSONSchema)
  deriving (Show, Eq)

-- MaxItems, MinItems, isUnique, Items
data ArrProperties
  = AP
      (Maybe Int)
      (Maybe Int)
      Bool
      (Maybe HSONSchema)
  deriving (Show, Eq)

type Attribute = String

------------------------- HSON to HSON Schema  ---------------------)-----------

-- | converts a HSON object property to an int property
numberHelper :: HSON -> HSONSchema
numberHelper x = undefined

-- Int (IP - - - -)
intHelper :: HSON -> HSONSchema
intHelper x = undefined

-- Str (SP - - -)
stringHelper :: HSON -> HSONSchema
stringHelper x = undefined

-- Bool
boolHelper :: HSON -> HSONSchema
boolHelper x = undefined

-- Arr (AP - - - -)
arrHelper :: HSON -> HSONSchema
arrHelper x = undefined

-- Obj (OP - - - -)
objHelper :: HSON -> HSONSchema
objHelper x = undefined

--
hsonToHSONSchema :: HSON -> HSONSchema
hsonToHSONSchema = undefined
