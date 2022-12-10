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

genHSON :: Gen HSON
genHSON = suchThatMap (listOf $ (,) <$> listOf genChar <*> genValue) (Just . H)

-- >>> sample' genHSON
-- [H [],H [("d",Integer 2)],H [],H [],H [],H [("acdbaabddb",Integer (-1)),("bdbbbadd",Number 5.419769396423644),("cbacdbbdcd",Array [Integer 7,Null,Number (-6.309052398448904)]),("a",String "daddaccdc")],H [("baccb",Array [String "dccd",Number 7.036215475402167,Null]),("b",Boolean True),("dcbcaa",Number (-1.6745653868266517)),("bbdcba",Boolean True)],H [("bcbaaabbbab",String "abbccccd"),("d",Number (-1.9912971988700998)),("acabd",Integer (-4)),("daca",String "cd"),("dacccabdb",Integer (-9)),("cbcbcbbbbc",String "ddccadcacb"),("daddaccacccbb",Number (-1.2154405815488316)),("cabccbd",Array [String "",Integer (-2)]),("bbddadcbcddbb",Number (-10.090027451378099))],H [("bd",Boolean True),("bbcb",Boolean False),("adccdddcacc",Array [Number 5.0489765947607355,String "aca",String "bbbabd"]),("ba",Array [String "abccbac",Null]),("ddddbbaacacbc",Number (-1.1579474113409751)),("b",Number (-14.30603279065375)),("cd",String "bdabcbdbbbdccd"),("bbab",Boolean True)],H [("cbda",String "ccabdabdadddc"),("cbbadc",String "bdccdbbb"),("dcbca",Integer 0),("acccadcadabacc",Number 13.396926741446698),("bddbbbdca",Array [String "a",Integer 3]),("cacbdbaadbdd",Object (H [("ad",Integer (-8)),("bdddacccbb",Integer 8),("acb",Array [Boolean True]),("addcadacd",String "d"),("acaaacd",Integer 16)])),("cadbbcdcd",Array [Number 2.068690813217339,Array [Number 16.766985477061727,Boolean True],String "dbabaddabb"]),("acdacbadbc",String "aacdccbcddcccd")],H [("accacacdbcbdddd",Boolean False),("dcadbbcbdbadbbcddcd",String "ddcddbbadc"),("adad",Boolean True),("",Boolean True),("",String "bbdcdcbadab"),("cbbadaaadc",String "ddbbadcdcbcbacadcdd"),("cabddcdacdabaa",String "ddbadabcabcbcac"),("aacbdbaaddbbcbbbddbc",Integer 3),("aadda",Number 14.831724891424164),("cadaa",Array [Array [Array [Array [],Number 17.28425820586969,Boolean True,Array [Number 0.3973095566824386,String "dcda",String "adacaaaabdbbcabddc",Integer 16]],Array [Number 9.447035879363131,String "cbab"],String "cabdbddadca",Array [Boolean False]],Number (-12.431531380035246),Integer (-9),String "dbdaacdccbcb",String "dddb"]),("bc",Boolean True),("acbcbd",String "ccbadcbccdbccaac"),("dcababba",String ""),("b",String "cdcbcdadabccdacaccac"),("dbbdaabcbaab",Boolean False),("caaad",Number (-1.3652439282040292)),("bababdcc",Array [Integer 7,String "dbaaa",Array [Number 5.922690866134423,Array [Array [Integer 12],Integer (-2),Array [Number (-19.658432240393154),String "c",String "ddcacab",Integer 9]],Array [Integer 3,String "bbcadaacdcdd",Boolean True,String "dcbbaaaaaaaaadbc",Number (-0.7629940550084482)]],Number 0.9542656017484699,Number (-2.6271455995465223)])]]

instance Arbitrary Value where
  arbitrary = genValue

  shrink (Object (H (x : xs))) = Object <$> [H [x], H xs]
  shrink (Array (x : xs)) = Array <$> [[x], xs]
  shrink x = []

instance Arbitrary Key where
  arbitrary = listOf genChar
  shrink x = []

instance Arbitrary HSON where
  arbitrary = genHSON

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
