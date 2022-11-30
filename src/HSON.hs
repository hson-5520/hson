module HSON where

import Control.Monad.Except
import Data.Map

data Value
  = Number Double
  | Object HSON
  | Boolean Bool
  | Array [Value]
  | Null
  | String String

type Key = String

data HSON
  = Map Key Value
  | Empty

instance Semigroup HSON where
  a <> b = case (a, b) of
    (Empty, Empty) -> Empty
    (Empty, x) -> x
    (x, Empty) -> x
    (x, y) -> x

instance Monoid HSON where
  mempty = Empty