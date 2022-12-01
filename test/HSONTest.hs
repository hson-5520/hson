module HSONTest where

import Control.Applicative
import Data.Map qualified as Map
import FromJSON
import HSON (HSON (Empty, Map), Key, Value (Array, Boolean, Null, Number, Object, String), hsonArray, hsonDog, hsonEmpty, hsonSchool, hsonSingle)
import Lib
import Parser qualified as P
import Test.HUnit (Test (TestList), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck

-------------------------- HSON Objects ------------------------------------

-- >>> hsonSingle

--------------------------------------------------------------------------------
