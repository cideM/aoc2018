module Types where

import qualified Data.Map.Strict as M'
import           Data.Ord        (Down (..), comparing)
import           Data.Text       (Text)

newtype TeamName =
  TeamName Text
  deriving (Show, Eq)

newtype GroupID =
  GroupID Text
  deriving (Show, Ord, Eq)

data DamageType
  = Fire
  | Cold
  | Radiation
  | Slashing
  | Bludgeoning
  deriving (Eq, Show)

data Attribute
  = WeakTo DamageType
  | ImmuneTo DamageType
  deriving (Eq, Show)

data Group a = Group
  { _unitCount        :: a
  , _hitPointsPerUnit :: a
  , _initiative       :: a
  , _damage           :: a
  , _damageType       :: DamageType
  , _attributes       :: [Attribute]
  , _team             :: TeamName
  } deriving (Eq, Show)

getEP :: (Num a) => Group a -> a
getEP g = _unitCount g * _damage g

instance (Ord a, Num a) => Ord (Group a) where
  g1 `compare` g2 = mconcat [comparing getEP g1 g2, comparing _initiative g1 g2]

type Groups a = M'.Map GroupID (Group a)

type GameState = Groups Int
