module HSON (HSON, Key, Value (Boolean, Number, String, Array, Object, Null, Integer), hsonEmpty, hsonSchool, hsonDog, hsonArray, hsonSingle) where

import Control.Monad qualified as Monad
import Data.Map
import Data.Map qualified as Map
import GHC.Arr (Array)
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

genValue :: Gen Value
genValue =
  frequency
    [ (8, fmap String (arbitrary :: Gen String)),
      (4, fmap Integer (arbitrary :: Gen Int)),
      (4, fmap Number (arbitrary :: Gen Double)),
      (3, fmap Boolean (arbitrary :: Gen Bool)),
      (2, fmap Array (genList :: Gen [Value])),
      (1, fmap Object genHSON),
      (1, return Null)
    ]

instance Arbitrary Value where
  arbitrary = genValue
  shrink (Object (x : xs)) = Object <$> ([x] : [xs])
  shrink (Array (x : xs)) = Array <$> ([x] : [xs])
  shrink x = []

genList :: forall a. (Arbitrary a) => Gen [a]
genList = sized gen
  where
    gen :: Int -> Gen [a]
    gen n =
      frequency
        [ (1, return []),
          (n, Monad.liftM2 (:) arbitrary (gen (n `div` 2)))
        ]

-- >>> sample' genValue
-- [String "",Array [],Integer 2,Object [("\b/",Integer (-5)),("q\GSp/\t",String "ltf")],Object [("uDq=e\RS<0",Null),("`\USk",Null)],Integer 0,String "Cy\133737K<;\54426\&5%,",Number 5.811902573451713,String "\ESCQ)A\1097422N\SOH\1078443t\f0",Boolean True,String "V\1111787>[\60881I\153675i\43596\77855y\US\NUL\1076314V\ESC"]

genHSON :: Gen HSON
genHSON = genList :: Gen [(Key, Value)]

instance Arbitrary HSON where
  arbitrary = genHSON

  shrink (x : xs) = [x] : [xs]
  shrink [] = []

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
