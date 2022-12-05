module HSONQuickCheck where

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

-- >>> hsonToString [("",Number 2.6979142905544144e-2)]
-- "{\"\": 2.6979142905544144e-2}"

-- >>> parse hsonP "{ \"bob\" : \"\" }"

-- >>> hsonToString [("",String "\USf\36038\"")]
-- "{\"\": \"\USf\36038\"\"}"

-- >>> hsonToString [("\"",Boolean False)]
-- "{\"\"\": false}"

-- >>> hsonToString [("",Number (-3.300495127670037e-2))]
-- "{\"\": -3.300495127670037e-2}"
