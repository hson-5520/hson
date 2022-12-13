module HSONSchema
  ( HSONSchema (..),
    IntProperties (..),
    NumProperties (..),
    StrProperties (..),
    ArrProperties (..),
    ObjProperties (..),
    BoolProperties (..),
  )
where

import Data.Map
import Data.Map qualified as Map
import HSON (HSON, Key, Value (Array, Boolean, Null, Number, Object, String))
import Parser qualified as P

------------------------- Defining HSON Schema  --------------------------------

-- | datatype representing an HSONSchema object
data HSONSchema
  = Str StrProperties -- -> (Value -> Bool)
  | Int IntProperties
  | Num NumProperties
  | Bool BoolProperties
  | Arr ArrProperties
  | Obj ObjProperties
  | Nul
  deriving (Show, Eq)

-- | datatype representing the properties of an int
data IntProperties = IP
  { iMinimum :: Maybe Int,
    iMaximum :: Maybe Int,
    iExclusiveMinimum :: Maybe Int,
    iExclusiveMaximum :: Maybe Int,
    iMultipleOf :: Maybe Int,
    intEnum :: Maybe [Int]
  }
  deriving (Show, Eq)

-- | datatype representing the properties of a num
data NumProperties = NP
  { nMinimum :: Maybe Double,
    nMaximum :: Maybe Double,
    nExclusiveMinimum :: Maybe Double,
    nExclusiveMaximum :: Maybe Double,
    nMultipleOf :: Maybe Double,
    numberEnum :: Maybe [Double]
  }
  deriving (Show, Eq)

-- | datatype representing the properties of a string
data StrProperties = SP
  { minLength :: Maybe Int,
    maxLength :: Maybe Int,
    pattern :: Maybe String,
    stringEnum :: Maybe [String]
  }
  deriving (Show, Eq)

-- | datatype representing the properties of a boolean
data BoolProperties = BP {boolEnum :: Maybe Bool}
  deriving (Show, Eq)

-- | datatype representing the properties of an array
data ArrProperties = AP
  { minItems :: Maybe Int,
    maxItems :: Maybe Int,
    isUnique :: Bool,
    items :: Maybe HSONSchema
  }
  deriving (Show, Eq)

-- | datatype representing the properties of an object
data ObjProperties = OP
  { minProperties :: Maybe Int,
    maxProperties :: Maybe Int,
    required :: [String],
    properties :: [(Key, HSONSchema)]
  }
  deriving (Show, Eq)