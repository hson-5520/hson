module HSONValidator where

import HSON
import HSONSchema (ArrProperties, HSONSchema, IntProperties, ObjProperties, StrProperties)

------------------------- Validating HSON Values  ------------------------------

-- newtype Schema = { validate :: HSON -> Bool }

-- -- definition of the parser type
-- newtype Schema a = S {validate :: a -> HProperty -> Bool}

-- instance Functor Schema where
--   fmap :: (a -> b) -> Parser a -> Parser b
--   fmap f p = P $ \s -> do
--     (c, cs) <- doParse p s
--     return (f c, cs)

-- instance Applicative Parser where
--   pure :: a -> Parser a
--   pure x = P $ \s -> Just (x, s)

--   (<*>) :: Parser (a -> b) -> Parser a -> Parser b
--   p1 <*> p2 = P $ \s -> do
--     (f, s') <- doParse p1 s
--     (x, s'') <- doParse p2 s'
--     return (f x, s'')

-- instance Alternative Parser where
--   empty :: Parser a
--   empty = P $ const Nothing

--   (<|>) :: Parser a -> Parser a -> Parser a
--   p1 <|> p2 = P $ \s -> doParse p1 s `firstJust` doParse p2 s

-- | Check if a non-integer number meets its required properties
validateNum :: Double -> IntProperties -> Bool
validateNum = undefined

-- | Check if an integer meets its required properties
validateInt :: Int -> IntProperties -> Bool
validateInt = undefined

-- | Check if a string meets its required properties
validateString :: String -> StrProperties -> Bool
validateString = undefined

-- | Check if an array meets its required properties
validateArr :: [Value] -> ArrProperties -> Bool
validateArr = undefined

-- | Check if an object meets its required properties
validateObj :: HSON -> ObjProperties -> Bool
validateObj = undefined

------------------------- Validating HSON  -------------------------------------

validateHSON :: HSON -> HSONSchema -> Bool
validateHSON = undefined
