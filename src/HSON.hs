module HSON (HSON, Key, Value (Boolean, Number, String, Array, Object, Null, Integer), hsonEmpty, hsonSchool, hsonDog, hsonArray, hsonSingle) where

import Control.Monad qualified as Monad
import Data.Map
import Data.Map qualified as Map
import GHC.Arr (Array)
import Parser qualified as P
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    frequency,
    listOf,
    oneof,
    sample',
    sized,
  )

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

-- newtype HSON = HSON [(Key, Value)] deriving (Eq, Show)

------------------------- HSON Generator  ----------------------------------

genList :: forall a. (Arbitrary a) => Gen [a]
genList = sized gen
  where
    gen :: Int -> Gen [a]
    gen n =
      frequency
        [ (1, return []),
          (n, Monad.liftM2 (:) arbitrary (gen (n `div` 2)))
        ]

genChar :: Gen Char
genChar = oneof [return 'a', return 'b', return 'c', return 'd']

genValue :: Gen Value
genValue =
  frequency
    [ (8, fmap String (listOf genChar)),
      (4, fmap Integer (arbitrary :: Gen Int)),
      (4, fmap Number (arbitrary :: Gen Double)),
      (3, fmap Boolean (arbitrary :: Gen Bool)),
      (5, fmap Array genList),
      (1, fmap Object genHSON),
      (1, return Null)
    ]

-- >>> sample' genValue
-- [Integer 0,Number 1.482038566185,Number (-0.18688516311461165),Number 5.6291743727845,Null,String "ddbaadbc",Number 1.0030608518726052,String "d",Number (-11.369217630535154),Integer 6,Integer (-8)]

-- genHSON :: Gen HSON
-- genHSON = genList :: Gen [(Key, Value)]

genHSON :: Gen HSON
genHSON = listOf $ (,) <$> listOf genChar <*> genValue

-- >>> sample' genHSON

instance Arbitrary Value where
  arbitrary = genValue
  shrink (Object (x : xs)) = Object <$> ([x] : [xs])
  shrink (Array (x : xs)) = Array <$> ([x] : [xs])
  shrink x = []

-- instance Arbitrary Key where
--   arbitrary = listOf genChar
--   shrink x = []

-- instance Arbitrary HSON where
--   arbitrary = genHSON

--   shrink (x : xs) = [x] : [xs]
--   shrink [] = []

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
