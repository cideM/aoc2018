module Day20.Types where

import Data.Vector (Vector)

data Path
  = Step Direction
  | Branches [[Path]]
  deriving (Show)

data Direction
  = N
  | S
  | W
  | E
  deriving (Show, Eq)
