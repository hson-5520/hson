module HSON (HSON (H), Key, Value (Boolean, Number, String, Array, Object, Null, Integer), hsonEmpty, hsonSchool, hsonDog, hsonArray, hsonSingle) where

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
    suchThatMap,
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

newtype HSON = H [(Key, Value)] deriving (Eq, Show)

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

genHSON :: Gen HSON
genHSON = suchThatMap (listOf $ (,) <$> listOf genChar <*> genValue) (Just . H)

-- >>> sample' genHSON

instance Arbitrary Value where
  arbitrary = genValue

  -- shrink (Object (H (x : xs))) = Object <$> [H [x], H xs]
  -- shrink (Array (x : xs)) = Array <$> [[x], xs]
  shrink x = []

instance Arbitrary Key where
  -- arbitrary = listOf genChar
  shrink x = []

instance Arbitrary HSON where
  arbitrary = genHSON

  -- shrink (H (x : xs)) = [H [x], H xs]
  shrink (H []) = []

---------------------------- Sample HSON ---------------------------------------
hsonEmpty :: HSON
hsonEmpty = H []

hsonSingle :: HSON
hsonSingle = H [("name", String "bob")]

hsonArray :: HSON
hsonArray =
  H
    [ ("bob", Array [Integer 1, String "hi", Object $ H [("name", String "Jose")], Null])
    ]

hsonDog :: HSON
hsonDog =
  H
    [ ( "dog",
        Object $
          H
            [ ("name", String "bill"),
              ("age", Number 4.2),
              ("siblings", Boolean False)
            ]
      )
    ]

hsonSchool :: HSON
hsonSchool =
  H
    [ ("name", String "School"),
      ("foundedYear", Integer 1975),
      ("isPublic", Boolean True),
      ("cost", Null),
      ("students", Array [String "a", String "b", String "c"]),
      ( "address",
        Object $
          H
            [ ("city", String "Philadelphia"),
              ("state", String "Pennsylvania"),
              ("buildingNumber", Integer 123)
            ]
      )
    ]

---------------------------------------------------------------------------------
