module HSON (HSON (H), Key, Value (Boolean, Number, String, Array, Object, Null, Integer), hsonEmpty, hsonSchool, hsonDog, hsonArray, hsonSingle) where

import Control.Monad qualified as Monad
import Data.Map
import Data.Map qualified as Map
import GHC.Arr (Array)
import Parser qualified as P
import Test.QuickCheck
  ( ASCIIString (getASCIIString),
    Arbitrary (..),
    Gen,
    PrintableString (getPrintableString),
    UnicodeString (UnicodeString),
    elements,
    frequency,
    listOf,
    oneof,
    resize,
    sample',
    scale,
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

genChar :: Gen Char
genChar = elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'])

genValue :: Gen Value
genValue =
  frequency
    [ (12, fmap String (vectorOf 5 genChar)),
      (12, fmap Integer (arbitrary :: Gen Int)),
      (12, fmap Number (arbitrary :: Gen Double)),
      (12, fmap Boolean (arbitrary :: Gen Bool)),
      (2, fmap Array genList),
      (1, fmap Object genHSON),
      (12, return Null)
    ]

-- >>> sample' genValue
-- [Boolean True,Number (-1.5022829171397718),Integer 0,Null,Boolean True,Boolean False,Null,Integer (-7),Integer 16,Object (H [("ccbcdcdbba",Integer (-6)),("b",Null),("cacb",Boolean True),("bcaccca",Number (-4.135474729261158)),("adcaabbacdac",Integer (-18)),("cacccdabaaabdaa",String "\\[T=3CH\176067(e"),("",Integer (-10)),("bccacdda",Boolean False),("bbdcdabadaddabca",Boolean False),("cbacadccbbcaccbbbd",Number (-15.302382184328971)),("d",Null),("",Boolean True),("bcadbbb",Boolean True),("dbabdccaccd",Number 1.8869898049196792),("abcabb",Number 7.4422110660045835),("dcabdcbabab",String "~7z\78883\&4}p|^beO"),("cadadcaabcacbbbdbd",String ".lK[\22108\134276u$ PdP\40178<"),("dadcadacdbbbdcaa",Null)]),Integer 13]

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
