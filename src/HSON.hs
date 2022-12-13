module HSON
  ( HSON (H),
    Key,
    Value (Boolean, Number, String, Array, Object, Null, Integer),
    hsonEmpty,
    hsonSchool,
    hsonDog,
    hsonArray,
    hsonSingle,
  )
where

import Control.Monad qualified as Monad
import Parser qualified as P
import System.Random
import Test.QuickCheck
  ( Arbitrary (..),
    Gen,
    elements,
    frequency,
    listOf,
    sample',
    sized,
    suchThatMap,
    vectorOf,
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

genString :: Gen String
genString = vectorOf 5 (elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']))

genValue :: Gen Value
genValue =
  frequency
    [ (12, fmap String genString),
      (12, fmap Integer (arbitrary :: Gen Int)),
      (12, fmap Number (arbitrary :: Gen Double)),
      (12, fmap Boolean (arbitrary :: Gen Bool)),
      (4, fmap Array genList),
      (4, fmap Object genHSON),
      (12, return Null)
    ]

genHSON :: Gen HSON
genHSON = suchThatMap (vectorOf 10 $ (,) <$> genString <*> genValue) (Just . H)

instance Arbitrary Value where
  arbitrary :: Gen Value
  arbitrary = genValue

  shrink :: Value -> [Value]
  shrink (Object (H (x : xs))) = Object <$> [H [x], H xs]
  shrink (Array (x : xs)) = Array <$> [[x], xs]
  shrink x = []

instance Arbitrary HSON where
  arbitrary :: Gen HSON
  arbitrary = genHSON

  shrink :: HSON -> [HSON]
  shrink (H (x : xs)) = [H [x], H xs]
  shrink (H []) = []

---------------------------- Sample HSON ---------------------------------------
hsonEmpty :: HSON
hsonEmpty = H []

hsonSingle :: HSON
hsonSingle = H [("name", String "bob")]

hsonArray :: HSON
hsonArray =
  H
    [ ( "bob",
        Array
          [ Integer 1,
            String "hi",
            Object $ H [("name", String "Jose")],
            Null
          ]
      )
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
