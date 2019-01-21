{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Day20.Run
  ( run
  ) where

import           Data.Array      ((!))
import qualified Data.Array      as Array
import qualified Data.Foldable   as Foldable
import           Data.Function   (on)
import qualified Data.List       as List
import qualified Data.List.Extra as Extra
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Semigroup  (Max (..), Min (..))
import           Data.Sequence   (Seq)
import qualified Data.Sequence   as Seq
import           Data.Set        (Set)
import qualified Data.Set        as Set
import           Data.Text       (Text)
import qualified Data.Text       as Text
import qualified Data.Text.IO    as TIO
import           Day20.Parser    (parseInput)
import           Day20.Types
import           Debug.Trace
import           Types

flattenPaths :: [Path] -> [[Direction]]
flattenPaths paths = go paths [[]]
  where
    go [] out = out
    go (Step dir:rest) acc =
      let acc' = (++) <$> acc <*> [[dir]]
       in go rest acc'
    go (Branches as:rest) acc =
      let acc' = (++) <$> acc <*> concatMap flattenPaths as
       in go rest acc'

go :: Point -> Direction -> Point
go (Point (x, y)) N = Point (x, y - 1)
go (Point (x, y)) E = Point (x + 1, y)
go (Point (x, y)) W = Point (x - 1, y)
go (Point (x, y)) S = Point (x, y + 1)

makeWorld :: [[Direction]] -> World
makeWorld =
  foldr1 Map.union .
  map (fst . List.foldl' f (Map.fromList [(origin, Room)], origin))
  where
    origin = Point (0, 0)
    f (worldMap, loc) dir =
      let firstStep = go loc dir
          secondStep = go firstStep dir
          map' = Map.insertWith (flip const) firstStep Door worldMap
       in (Map.insertWith (flip const) secondStep Room map', secondStep)

worldToGrid :: World -> Grid
worldToGrid w =
  let bounds =
        foldMap (\(Point (x, y), _) -> Bounds (Min x) (Min y) (Max x) (Max y)) $
        Map.assocs w
      minPoint@(Point (x0, y0)) =
        Point (getMin $ _minX bounds, getMin $ _minY bounds)
      maxPoint@(Point (x1, y1)) =
        Point (getMax $ _maxX bounds, getMax $ _maxY bounds)
   in Array.array
        (minPoint, maxPoint)
        [ let p = Point (x, y)
           in (p, Map.findWithDefault Wall p w)
        | y <- [y0 .. y1]
        , x <- [x0 .. x1]
        ]

inputToWorld :: [[Path]] -> World
inputToWorld = foldr1 Map.union . map (makeWorld . flattenPaths)

printGrid :: Grid -> IO ()
printGrid grid =
  let (Point (x, _), Point (x', _)) = Array.bounds grid
      width = (x' - x) + 1
   in mapM_ (TIO.putStrLn . Text.unwords) .
      Extra.chunksOf width . map (Text.pack . show . snd) $
      List.sortBy (compare `on` fst) (Array.assocs grid)

getNeighbours :: Grid -> Point -> [Point]
getNeighbours grid (Point (x, y)) =
  let (Point (x0, y0), Point (x1, y1)) = Array.bounds grid
      isInGrid (Point (x', y')) = x' >= x0 && x' <= x1 && y' >= y0 && y' <= y1
   in filter
        isInGrid
        [Point (x, y + 1), Point (x, y - 1), Point (x + 1, y), Point (x - 1, y)]

distances :: Grid -> Point -> Map Point Int
distances g origin = go Map.empty $ Seq.singleton (origin, 0)
  where
    getNs = getNeighbours g
    notWall p = (g ! p) /= Wall
    go seen Seq.Empty = seen
    go seen (p@(point, dist) Seq.:<| rest)
      | Map.member point seen = go (traceShowId seen) rest
      | otherwise = go seen' queue'
      where
        seen' = Map.insert point (traceShowId dist) seen
        queue' =
          rest Seq.><
          (traceShowId . Seq.fromList . map (, dist + 1) . filter notWall $ getNs point)

run :: Text -> Either ErrMsg Text
run input = do
  paths <- parseInput input
  let world = inputToWorld paths
      grid = worldToGrid world
      dists = distances grid (Point (0,0))
      maxDist = getMax . foldMap Max $ Map.elems dists
  return . Text.pack $ "p1: " ++ show (maxDist `div` 2)
