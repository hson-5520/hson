module HSON (HSON, Key, Value (Boolean, Number, String, Array, Object, Null, Integer), hsonEmpty, hsonSchool, hsonDog, hsonArray, hsonSingle) where

import Data.Map
import Data.Map qualified as Map
import Parser qualified as P
import Test.QuickCheck

------------------------- Defining HSON  ----------------------------------

data Value
  = String String
  | Integer Int
  | Number Double
  | Boolean Bool
  | Array [Value]
  | Object HSON
  | Null
  deriving (Eq, Show)

type Key = String

type HSON = [(Key, Value)]

------------------------- HSON Generator  ----------------------------------

genHSON :: Gen HSON
genHSON = undefined

instance Arbitrary HSON where
  arbitrary = genHSON

  shrink x = undefined

---------------------------- Sample HSON ---------------------------------------
hsonEmpty :: HSON
hsonEmpty = []

hsonSingle :: HSON
hsonSingle = [("name", String "bob")]

hsonArray :: HSON
hsonArray =
  [ ("bob", Array [Integer 1, String "hi", Object [("name", String "Jose")], Null])
  ]

hsonDog :: HSON
hsonDog =
  [ ( "dog",
      Object
        [ ("name", String "bill"),
          ("age", Number 4.2),
          ("siblings", Boolean False)
        ]
    )
  ]

hsonSchool :: HSON
hsonSchool =
  [ ("name", String "School"),
    ("foundedYear", Integer 1975),
    ("isPublic", Boolean True),
    ("cost", Null),
    ("students", Array [String "a", String "b", String "c"]),
    ( "address",
      Object
        [ ("city", String "Philadelphia"),
          ("state", String "Pennsylvania"),
          ("buildingNumber", Integer 123)
        ]
    )
  ]

---------------------------------------------------------------------------------