module HSON
  ( HSON (H),
    Key,
    Value (Boolean, Number, String, Array, Object, Null, Integer),
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
  = Integer Int
  | Number Double
  | String String
  | Boolean Bool
  | Array [Value]
  | Object HSON
  | Null
  deriving (Eq, Show)

type Key = String

newtype HSON = H [(Key, Value)] deriving (Eq, Show)

------------------------- HSON Generator  ----------------------------------

-- | generates a list of objects of the specified type
genList :: forall a. (Arbitrary a) => Gen [a]
genList = sized gen
  where
    gen :: Int -> Gen [a]
    gen n =
      frequency
        [ (1, return []),
          (n, Monad.liftM2 (:) arbitrary (gen (n `div` 2)))
        ]

-- | generates arbitrary alphanumeric strings of length 5
genString :: Gen String
genString = vectorOf 5 (elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']))

-- | generates an element of the Value type
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

-- | generates an arbitrary HSON object
genHSON :: Gen HSON
genHSON = suchThatMap (vectorOf 10 $ (,) <$> genString <*> genValue) (Just . H)

-- | arbitrary instance for the Value type
instance Arbitrary Value where
  arbitrary :: Gen Value
  arbitrary = genValue

  shrink :: Value -> [Value]
  shrink (Object (H (x : xs))) = Object <$> [H [x], H xs]
  shrink (Array (x : xs)) = Array <$> [[x], xs]
  shrink x = []

-- | arbitrary instance for the HSON type
instance Arbitrary HSON where
  arbitrary :: Gen HSON
  arbitrary = genHSON

  shrink :: HSON -> [HSON]
  shrink (H (x : xs)) = [H [x], H xs]
  shrink (H []) = []

--------------------------------------------------------------------------------