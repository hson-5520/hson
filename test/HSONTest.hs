module HSONTest where

import Control.Applicative
import Data.Map qualified as Map
import FromJSON
import HSON (HSON (Empty, Map), Key, Value (Array, Boolean, Null, Number, Object, String), hsonArray, hsonDog, hsonEmpty, hsonSchool, hsonSingle)
import Lib
import Parser
import Parser qualified as P
import Test.HUnit (Test (TestList), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck
import ToJSON

---------------------------- QuickCheck: toJSON fromJSON  ---------------------------------------

-- >>> quickCheck prop_toAndFrom

-- prop_toAndFrom :: HSON -> Property
-- prop_toAndFrom x =
--   let y = helper x
--    in property (y == x)
--   where
--     helper x = do
--       toJSON "HSONTestFile.txt" x
--       z <- parseJSON "HSONTestFile.txt"
--       case z of
--         Right x -> return x
--         Left y -> return Empty
