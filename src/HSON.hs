module HSON (Key, Value (Boolean, Number, String), HSON) where

import Control.Monad.Except
import Data.Map
import Parser qualified as P

data Value
  = String String
  | Number Double
  | Boolean Bool
  | Object HSON
  | Array [Value]
  | Null
  deriving (Eq, Show)

type Key = String

data HSON
  = Map Key Value
  | Empty
  deriving (Eq, Show)

instance Semigroup HSON where
  a <> b = case (a, b) of
    (Empty, Empty) -> Empty
    (Empty, x) -> x
    (x, Empty) -> x
    (x, y) -> x

instance Monoid HSON where
  mempty = Empty
