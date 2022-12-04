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

type HSON = (Map Key Value)

------------------------- HSON Generator  ----------------------------------

genHSON :: Gen HSON
genHSON = undefined

instance Arbitrary HSON where
  arbitrary = genHSON

  shrink x = undefined

---------------------------- Sample HSON ---------------------------------------
hsonEmpty :: HSON
hsonEmpty = empty

hsonSingle :: HSON
hsonSingle = Map.fromList [("name", String "bob")]

hsonArray :: HSON
hsonArray =
  Map.fromList
    [ ("bob", Array [Integer 1, String "hi", Object (Map.fromList [("name", String "Jose")]), Null])
    ]

hsonDog :: HSON
hsonDog =
  Map.fromList
    [ ( "dog",
        Object
          ( Map.fromList
              [ ("age", Number 4.2),
                ("name", String "Bill"),
                ("siblings", Boolean False)
              ]
          )
      )
    ]

hsonSchool :: HSON
hsonSchool =
  Map.fromList
    [ ( "address",
        Object
          ( Map.fromList
              [ ("buildingNumber", Integer 123),
                ("city", String "Philadelphia"),
                ("state", String "Pennsylvania")
              ]
          )
      ),
      ("cost", Null),
      ("foundedYear", Integer 1975),
      ("isPublic", Boolean True),
      ("name", String "school"),
      ("students", Array [String "a", String "b", String "c"])
    ]

-- Right (fromList [("address",Object (fromList [("buildingNumber",Integer 123),("city",String "Philadelphia"),("state",String "Pennsylvania")])),("cost",Null),("foundedYear",Integer 1975),("isPublic",Boolean True),("name",String "School"),("students",Array [String "a",String "b",String "c"])])

---------------------------------------------------------------------------------