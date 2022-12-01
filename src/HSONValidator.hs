module HSONValidator where

import HSON
import HSONSchema

-- | Check if an object meets its required properties
validateObj :: HSON -> ObjProperties -> Bool
validateObj = undefined

-- | Check if an integer meets its required properties
validateInt :: Int -> IntProperties -> Bool
validateInt = undefined

-- | Check if a string meets its required properties
validateString :: String -> StrProperties -> Bool
validateString = undefined

-- | Check if a non-integer number meets its required properties
validateNum :: Double -> IntProperties -> Bool
validateNum = undefined

-- | Check if an array meets its required properties
validateArr :: [Value] -> ArrProperties -> Bool
validateArr = undefined
