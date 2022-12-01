module HSON (Key, Value (Boolean, Number, String, Array, Object, Null), HSON (Map, Empty), hsonEmpty, hsonSchool, hsonDog, hsonArray, hsonSingle) where

import Data.Map
import Data.Map qualified as Map
import Parser qualified as P
import Test.QuickCheck

------------------------- Defining HSON  ----------------------------------

data Value
  = String String
  | Number Double
  | Boolean Bool
  | Array [Value]
  | Object HSON
  | Null
  deriving (Eq, Show)

type Key = String

data HSON
  = Map (Map Key Value)
  | Empty
  deriving (Eq, Show)

instance Semigroup HSON where
  a <> b = case (a, b) of
    (Empty, Empty) -> Empty
    (Empty, x) -> x
    (x, Empty) -> x
    (x, y) -> x

instance Monoid HSON where
  mempty = Empty

------------------------- HSON Generator  ----------------------------------

genHSON :: Gen HSON
genHSON =
  frequency
    [ (1, return Empty)
    ]

instance Arbitrary HSON where
  arbitrary = genHSON

  shrink (Map x) = undefined
  shrink Empty = [Empty]

---------------------------- Sample HSON ---------------------------------------
hsonEmpty :: HSON
hsonEmpty = Empty

hsonSingle :: HSON
hsonSingle = Map (Map.fromList [("name", String "bob")])

hsonArray :: HSON
hsonArray =
  Map
    ( Map.fromList
        [ ("bob", Array [Number 1, String "hi", Object $ Map (Map.fromList [("name", String "Jose")]), Null])
        ]
    )

hsonDog :: HSON
hsonDog =
  Map
    ( Map.fromList
        [ ( "dog",
            Object $
              Map
                ( Map.fromList
                    [ ("name", String "Bill"),
                      ("age", Number 4.2),
                      ("siblings", Boolean False)
                    ]
                )
          )
        ]
    )

hsonSchool :: HSON
hsonSchool =
  Map
    ( Map.fromList
        [ ("name", String "school"),
          ("foundedYear", Number 1975),
          ("isPublic", Boolean True),
          ("cost", Null),
          ("students", Array [String "a", String "b", String "c"]),
          ( "address",
            Object $
              Map
                ( Map.fromList
                    [ ("city", String "Philadelphia"),
                      ("state", String "Pennsylvania"),
                      ("buildingNumber", Number 123)
                    ]
                )
          )
        ]
    )

---------------------------------------------------------------------------------