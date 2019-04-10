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
import qualified Data.PQueue.Prio.Min as PQMin
import Data.Semigroup (Max(..), Min(..), (<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Void (Void)
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Bot = Bot
  { x :: !Int
  , y :: !Int
  , z :: !Int
  , r :: !Int
  } deriving (Show, Eq)

data BotAggregate = BotAggregate
  { maxX :: !(Max Int)
  , minX :: !(Min Int)
  , maxY :: !(Max Int)
  , minY :: !(Min Int)
  , maxZ :: !(Max Int)
  , minZ :: !(Min Int)
  } deriving (Show, Eq)

instance Semigroup BotAggregate where
  (<>) x y =
    BotAggregate
      { maxX = on (<>) maxX x y
      , minX = on (<>) minX x y
      , maxY = on (<>) maxY x y
      , minY = on (<>) minY x y
      , maxZ = on (<>) maxZ x y
      , minZ = on (<>) minZ x y
      }

instance Monoid BotAggregate where
  mappend = (<>)
  mempty =
    BotAggregate {maxX = 0, minX = 0, maxY = 0, minY = 0, maxZ = 0, minZ = 0}

data Point =
  Point !Int
        !Int
        !Int
  deriving (Show, Eq)

data Cube = Cube
  { origin :: Point
  , dx :: !Int
  , dy :: !Int
  , dz :: !Int
  } deriving (Show, Eq)

botParser :: Parser Bot
botParser = do
  _ <- string "pos=<"
  _x <- signedDec <* char ','
  _y <- signedDec <* char ','
  _z <- signedDec
  _ <- string ">, r="
  Bot _x _y _z <$> signedDec
  where
    signedDec = fromIntegral <$> L.signed space L.decimal

inputParser :: Parser (V.Vector Bot)
inputParser = V.fromList <$> many (botParser <* space)

distance :: Point -> Point -> Int
distance (Point x1 y1 z1) (Point x2 y2 z2) =
  let x' = abs $ x2 - x1
      y' = abs $ y2 - y1
      z' = abs $ z2 - z1
   in x' + y' + z'

main :: IO ()
main = do
  parsed <- runParser inputParser "stdin" . T.pack <$> getContents
  case parsed of
    Left e -> print e
    Right bots -> do
      let strongest = V.maximumBy (\b1 b2 -> r b1 `compare` r b2) bots
          inRangeOfStrongest =
            V.filter (flip (<=) (r strongest) . distBots strongest) bots
      print $ "Strongest: " ++ show strongest
      print $ "In range of strongest: " ++ show (V.length inRangeOfStrongest)
      print $ "P2 Distance Segments: " ++ show (p2alt bots)
      print $ "P2 Distance: Divide and Conquer " ++ show (p2 bots)
  where
    distBots b1 b2 =
      distance (Point (x b1) (y b1) (z b1)) (Point (x b2) (y b2) (z b2))

cubeFromBots :: V.Vector Bot -> Cube
cubeFromBots bots =
  let BotAggregate {minX, minY, minZ, maxX, maxY, maxZ} =
        foldMap makeBotAggregate bots
      minY' = getMin minY
      minX' = getMin minX
      minZ' = getMin minZ
   in Cube
        { origin = Point minX' minY' minZ'
        , dx = getMax maxX - minX'
        , dy = getMax maxY - minY'
        , dz = getMax maxZ - minZ'
        }
  where
    makeBotAggregate Bot {x, y, z, r} =
      BotAggregate
        { maxX = Max $ x + r
        , minX = Min $ x - r
        , maxY = Max $ y + r
        , minY = Min $ y - r
        , maxZ = Max $ z + r
        , minZ = Min $ z - r
        }

botReachesCube :: Bot -> Cube -> Bool
botReachesCube (Bot x y z r) (Cube (Point originX originY originZ) dx dy dz) =
  let closestPoint =
        Point
          (max originX (min x (originX + dx)))
          (max originY (min y (originY + dy)))
          (max originZ (min z (originZ + dz)))
      -- ^ Closest point in cube to bot
   in distance closestPoint (Point x y z) < r

partitionCube :: Cube -> V.Vector Cube
partitionCube (Cube (Point x y z) dx dy dz) =
  let dx' = dx `div` 2
      dy' = dy `div` 2
      dz' = dz `div` 2
   in V.fromList
        [ Cube (Point x y z) dx' dy' dz'
        , Cube (Point (x + dx') (y + dy') (z + dz')) dx' dy' dz'
        ]

getCubesReachedByMostBots ::
     V.Vector Bot -> V.Vector Cube -> Either (V.Vector Cube) Cube
getCubesReachedByMostBots bots cubes =
  let sorted =
        V.fromList .
        map fst .
        head .
        Extra.groupOn snd .
        List.sortBy
          (\(_, botsReaching) (_, botsReaching') ->
             comparing Down botsReaching botsReaching') .
        V.toList . V.zip cubes $
        V.map
          (\cube' -> V.length $ V.filter (`botReachesCube` cube') bots)
          cubes
   in if V.length sorted == 1
        then Right $ V.head sorted
        else Left sorted

getCubeClosestToOrigin :: V.Vector Cube -> Either (V.Vector Cube) Cube
getCubeClosestToOrigin cubes =
  let sorted =
        V.fromList .
        map fst .
        head .
        Extra.groupOn snd .
        List.sortBy
          (\(_, distanceFromOrigin) (_, distanceFromOrigin') ->
             compare distanceFromOrigin distanceFromOrigin') .
        V.toList . V.zip cubes $
        V.map
          (\(Cube cubeOrigin _ _ _) -> Point 0 0 0 `distance` cubeOrigin)
          cubes
   in if V.length sorted == 1
        then Right $ V.head sorted
        else Left sorted

data P2Result = P2Result
  { overlapCounter :: !Int
  , maxCount :: !Int
  , result :: !Int
  }

-- https://www.reddit.com/r/adventofcode/comments/a8s17l/2018_day_23_solutions/ecdqzdg/
p2alt :: V.Vector Bot -> Int
p2alt bots =
  let overlaps = List.foldl' getOverlaps PQMin.empty bots
   in result $
      PQMin.foldlWithKey getLowestDistanceFromOrigin (P2Result 0 0 0) overlaps
  where
    getOverlaps accumulator (Bot x y z r) =
      let distanceFromOrigin = distance (Point x y z) (Point 0 0 0)
          minDist = distanceFromOrigin - r
          maxDist = distanceFromOrigin + r
       in PQMin.insert (maxDist + 1) (-1) $
          PQMin.insert (max 0 minDist) 1 accumulator
    getLowestDistanceFromOrigin oldResult@P2Result { overlapCounter
                                                   , maxCount
                                                   , result
                                                   } currentDist endSegmentModifier =
      let count' = overlapCounter + endSegmentModifier
       in if count' > maxCount
            then P2Result count' count' currentDist
            else oldResult

-- https://raw.githack.com/ypsu/experiments/master/aoc2018day23/vis.html
p2 :: V.Vector Bot -> Int
p2 bots = do
  let cube =
        Cube
          (Point
             (-80000000000000000)
             (-80000000000000000)
             (-80000000000000000))
          160000000000000000
          160000000000000000
          160000000000000000
   in distance (Point 0 0 0) . origin $ go cube
  where
    go cube@(Cube _ 1 1 1) = cube
    go cube = do
      let partitioned = partitionCube cube
          byBotsReaching = getCubesReachedByMostBots bots partitioned
      case byBotsReaching of
        Right cube' -> go cube'
        Left cubes -> do
          let byDistanceFromOrigin = getCubeClosestToOrigin cubes
          case byDistanceFromOrigin of
            Left cubes' -> go $ V.head cubes'
            Right cube'' -> go cube''
