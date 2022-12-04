module HSONTest where

import Control.Applicative
import FromJSON
import HSON (HSON, Key, Value (Array, Boolean, Null, Number, Object, String), hsonArray, hsonDog, hsonEmpty, hsonSchool, hsonSingle)
import Lib
import Parser
import Test.HUnit (Test (TestList), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck
import ToJSON

---------------------------- QuickCheck: toJSON fromJSON  ---------------------------------------

-- >>> quickCheck prop_roundtrip

prop_roundtrip :: HSON -> Bool
prop_roundtrip hson =
  case helper hson of
    Nothing -> False
    Just y -> y == hson
  where
    helper :: HSON -> Maybe HSON
    helper x =
      let y = hsonToString hson
       in case parse hsonP y of
            Left _ -> Nothing
            Right x -> Just x
