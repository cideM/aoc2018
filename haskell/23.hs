#!/usr/bin/env stack
{-
    stack
    script
    --resolver lts-13.16
    --package transformers,mtl,megaparsec,text,vector,extra,pqueue
-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class (liftIO)
import Data.Function (on)
import qualified Data.List as List
import qualified Data.List.Extra as Extra
import Data.Ord (Down(..), comparing)
import qualified Data.PQueue.Prio.Max as PQMax
import Data.Semigroup (Max(..), Min(..), (<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Void (Void)
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

{-- 
   Solution should be 121493971 but mine is 120079817
--}
type Parser = Parsec Void Text

data Bot = Bot
  { _botCenter :: Point
  , _radius :: !Int
  } deriving (Eq, Show)

data Point =
  Point !Int
        !Int
        !Int
  deriving (Show, Eq)

data Cube = Cube
  { _topLeft :: Point
  , _length :: !Int
  } deriving (Show, Eq)

class HasPoint a where
  getPoint :: a -> Point

instance HasPoint Cube where
  getPoint (Cube point _) = point

instance HasPoint Point where
  getPoint = id

instance HasPoint Bot where
  getPoint (Bot point _) = point

origin :: Point
origin = Point 0 0 0

getCubeCenter :: Cube -> Point
getCubeCenter cube =
  let (Point x y z) = getPoint cube
   in Point
        (x + (_length cube) `div` 2)
        (y + (_length cube) `div` 2)
        (z + (_length cube) `div` 2)

-- | Manhattan distance between 2 points
getDistance :: Point -> Point -> Int
getDistance (Point x1 y1 z1) (Point x2 y2 z2) =
  let x' = abs $ x2 - x1
      y' = abs $ y2 - y1
      z' = abs $ z2 - z1
   in x' + y' + z'

botParser :: Parser Bot
botParser =
  Bot <$>
  (Point <$> (string "pos=<" *> signedDec <* char ',') <*>
   (signedDec <* char ',') <*>
   (signedDec <* string ">, r=")) <*>
  signedDec
  where
    signedDec = fromIntegral <$> L.signed space L.decimal

inputParser :: Parser (V.Vector Bot)
inputParser = V.fromList <$> many (botParser <* space)

bigCube1 =
  let point = Point (negate $ 2 ^ 59) (negate $ 2 ^ 59) (negate $ 2 ^ 59)
   in Cube point (2 ^ 60)

main :: IO ()
main = do
  parsed <- runParser inputParser "stdin" . T.pack <$> getContents
  case parsed of
    Left e -> print e
    Right bots -> do
      print $ "P1: " ++ show (p1 bots)
      print $ "P2: " ++ show (p2 bigCube1 bots)

p1 :: V.Vector Bot -> Int
p1 bots =
  let Bot strongestPoint strongestRadius =
        V.maximumBy (\bot1 bot2 -> _radius bot1 `compare` _radius bot2) bots
   in V.length $
      V.filter
        (\bot -> getDistance strongestPoint (_botCenter bot) <= strongestRadius)
        bots

newtype BotsReachingCube =
  BotsReachingCube Int
  deriving (Eq, Show, Ord)

newtype DistanceFromOrigin =
  DistanceFromOrigin Int
  deriving (Eq, Show)

instance Ord DistanceFromOrigin where
  DistanceFromOrigin d1 `compare` DistanceFromOrigin d2 =
    Down d1 `compare` Down d2

newtype CubeLength =
  CubeLength Int
  deriving (Eq, Show)

instance Ord CubeLength where
  CubeLength l1 `compare` CubeLength l2 = Down l1 `compare` Down l2

-- | This is the key used for the priority queue. Shorter cube length is 
-- better, more bots reaching the cube is better, and shorter distance from 
-- origin is better
newtype PrioKey =
  PrioKey (CubeLength, BotsReachingCube, DistanceFromOrigin)
  deriving (Ord, Eq, Show)

-- | Split the initial cube into smaller cubes. Select the cube that either has the
-- most bots reaching that cube or, if tied, the one that's closer to origin.
p2 :: Cube -> V.Vector Bot -> Int
p2 startCube bots =
  let distanceToOrigin = getDistance (getPoint startCube) origin
      botsReachingStartCube =
        V.length $ V.filter (botReachesCube startCube) bots
      pqueue =
        PQMax.singleton
          (PrioKey
             ( CubeLength $ _length startCube
             , BotsReachingCube botsReachingStartCube
             , DistanceFromOrigin distanceToOrigin))
          startCube
   in go pqueue
  where
      -- | Go over the smaller cubes, and discard the ones that aren't reached by any bots.
      -- Then insert the remaining cubes into the queue, together with the number of bots reaching each cube,
      -- the cube length (smaller cubes are prioritized) and the distance from origin.
    makeNextQueue accumulator currentCube =
      let botsReachingCube =
            V.length $ V.filter (botReachesCube currentCube) bots
          distanceFromOrigin = getDistance origin $ getCubeCenter currentCube
       in if botsReachingCube > 0
            then PQMax.insert
                   (PrioKey
                      ( CubeLength $ _length currentCube
                      , BotsReachingCube botsReachingCube
                      , DistanceFromOrigin distanceFromOrigin))
                   currentCube
                   accumulator
            else accumulator
    go pqueue =
      let (_, cube) = PQMax.findMax pqueue
       in if (_length cube) == 0
            then getDistance origin $ getPoint cube
            else go . V.foldl' makeNextQueue pqueue $ partitionCube cube

-- | Whether a cube is in range of a bot. It first selects the point in the 
-- cube that is closest to the bot. It then checks if the manhattan distance 
-- between that point and the bot center is smaller or equal to the bot radius
botReachesCube :: Cube -> Bot -> Bool
botReachesCube cube bot =
  let (Point originX originY originZ) = getPoint cube
      (Point x z y) = getPoint bot
      closestPoint =
        Point
          (max originX (min x (originX + _length cube)))
          (max originY (min y (originY + _length cube)))
          (max originZ (min z (originZ + _length cube)))
      -- ^ Closest point in cube to bot
   in getDistance closestPoint (Point x y z) <= _radius bot

-- | Split a cube into 8 smaller cubes. If the length of the cube is 1,
-- return all 8 points within that cube (== cubes of length 0)
partitionCube :: Cube -> V.Vector Cube
partitionCube (Cube (Point x y z) 1) =
  V.map (\p -> Cube p 0) $
  V.fromList
    [ Point x y z
    , Point (x + 1) y z
    , Point (x + 1) (y + 1) z
    , Point (x + 1) (y + 1) (z + 1)
    , Point (x + 1) y (z + 1)
    , Point x y (z + 1)
    , Point x (y + 1) z
    , Point x (y + 1) (z + 1)
    ]
partitionCube (Cube (Point x y z) cubeLength) =
  let newLength = cubeLength `div` 2
   in V.map (\p -> Cube p newLength) $
      V.fromList
        [ Point x y z
        , Point (x + newLength) y z
        , Point (x + newLength) (y + newLength) z
        , Point (x + newLength) (y + newLength) (z + newLength)
        , Point (x + newLength) y (z + newLength)
        , Point x y (z + newLength)
        , Point x (y + newLength) z
        , Point x (y + newLength) (z + newLength)
        ]
