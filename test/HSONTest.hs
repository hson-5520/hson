module HSONTest where

import Control.Applicative
import FromJSON
import HSON
  ( HSON,
    Key,
    Value (Array, Boolean, Null, Number, Object, String),
  )
import Parser
import Test.HUnit (Test (TestList), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck
import ToJSON

------------------------ QuickCheck: toJSON fromJSON  --------------------------

prop_hsonroundtrip :: HSON -> Bool
prop_hsonroundtrip hson =
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

--------------------------------------------------------------------------------
