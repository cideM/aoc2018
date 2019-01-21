module Day20.Types where

import           Data.Array      (Array)
import           Data.Ix         (Ix)
import           Data.Map.Strict (Map)
import           Data.Semigroup  (Max (..), Min (..))

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

newtype Point =
  Point (Int, Int) -- ^ x y
  deriving (Eq, Show, Bounded, Ix)

instance Ord Point where
  (Point (x, y)) `compare` (Point (x2, y2)) =
    if y2 /= y
      then y `compare` y2
      else x `compare` x2

type World = Map Point Cell

type Grid = Array Point Cell

data Cell
  = Room
  | Door
  | Wall
  deriving (Eq)

instance Show Cell where
  show Room = "."
  show Door = "~"
  show Wall = "#"

data Bounds = Bounds
  { _minX :: !(Min Int)
  , _minY :: !(Min Int)
  , _maxX :: !(Max Int)
  , _maxY :: !(Max Int)
  }

instance Monoid Bounds where
  mempty = Bounds (Min 0) (Min 0) (Max 0) (Max 0)

instance Semigroup Bounds where
  (<>) a b =
    Bounds
      { _minX = _minX a <> _minX b
      , _minY = _minY a <> _minY b
      , _maxX = _maxX a <> _maxX b
      , _maxY = _maxY a <> _maxY b
      }
