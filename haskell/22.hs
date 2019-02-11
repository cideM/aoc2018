#!/usr/bin/env stack
{-
    stack
    script
    --resolver lts-12.20
    --package containers,trifecta,unordered-containers,hashable,pqueue
-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

import           Data.Hashable        (Hashable)
import qualified Data.HashMap.Strict  as HashMap
import qualified Data.HashSet         as Set
import           Data.List            (foldl')
import qualified Data.List            as List
import qualified Data.Map.Strict      as Map
import qualified Data.Maybe           as Maybe
import qualified Data.PQueue.Prio.Min as PQ
import           Data.Semigroup       (Sum (..), getSum)
import           Debug.Trace
import           GHC.Generics         (Generic)
import           Text.Trifecta

type Coord = (Int, Int)

type Depth = Int

data TileType
  = Rock
  | Wet
  | Narrow
  deriving (Show, Eq)

data Tile = Tile
  { _geoIndex     :: !Int
  , _erosionLevel :: !Int
  , _tileType     :: TileType
  } deriving (Show, Eq)

type Cave = Map.Map Coord Tile

data Tool
  = Torch
  | Climb
  | Neither
  deriving (Show, Eq, Generic)

instance Hashable Tool

type Node = (Coord, Tool)

-- TODO: Use options throughout
data Options = Options
  { _depth  :: !Int
  , _width  :: !Int
  , _origin :: !Coord
  , _target :: !Coord
  , _maxX   :: !Int
  , _maxY   :: !Int
  }

-- depth: 8112
-- target: 13,743
parseInput :: String -> Either String (Depth, Coord)
parseInput str =
  case parseString p mempty str of
    Failure parseErr -> Left $ show parseErr
    Success x        -> Right x
  where
    p =
      (,) <$>
      (string "depth:" *> whiteSpace *> (fromIntegral <$> natural) <* whiteSpace) <*>
      (string "target: " *>
       ((,) <$> (fromIntegral <$> natural) <*>
        (char ',' *> (fromIntegral <$> natural))))

makeTile :: Options -> Cave -> Coord -> Tile
makeTile Options {..} cave current@(x, y) =
  let geoIndex = getGeoIndex
      erosionLevel = (geoIndex + _depth) `mod` 20183
      tileType =
        case erosionLevel `mod` 3 of
          0 -> Rock
          1 -> Wet
          2 -> Narrow
          _ -> error "Wrong erosion level"
   in Tile geoIndex erosionLevel tileType
  where
    getGeoIndex
      | current == _origin = 0
      | current == _target = 0
      | y == 0 = x * 16807
      | x == 0 = y * 48271
      | otherwise =
        _erosionLevel (cave Map.! (x, y - 1)) *
        _erosionLevel (cave Map.! (x - 1, y))

makeCave :: Options -> Cave
makeCave options@Options {..} =
  let coords = concat [[(x, y) | x <- [0 .. _width]] | y <- [0 .. _depth]]
      tiles = List.foldl' foldFn Map.empty coords
   in tiles
  where
    foldFn cave coord = Map.insert coord (makeTile options cave coord) cave

getRiskLevel :: Cave -> Coord -> Int
getRiskLevel cave (x, y) =
  getSum .
  foldMap
    (\(_, tile) ->
       case _tileType tile of
         Rock   -> Sum 0
         Wet    -> Sum 1
         Narrow -> Sum 2) .
  filter (\((x', y'), _) -> x' <= x && y' <= y) $
  Map.assocs cave

printRect :: Cave -> (Coord, Coord) -> IO ()
printRect cave ((x0, y0), (x1, y1)) =
  let coords = [[(x, y) | x <- [x0 .. x1]] | y <- [y0 .. y1]]
   in do print (cave Map.! (3, 1))
         mapM_
           (putStrLn . unwords . map (\coord -> printTile $ cave Map.! coord))
           coords
  where
    printTile tile =
      case _tileType tile of
        Rock   -> "."
        Wet    -> "="
        Narrow -> "|"

main :: IO ()
main =
  let origin = (0, 0)
   in do input <- parseInput <$> getContents
         case input of
           Left parseErr -> print parseErr
           Right (depth, target) -> do
             let opts =
                   Options
                     depth
                     (fst target)
                     origin
                     target
                     (fst target * 2)
                     (depth * 2)
                 cave = makeCave opts
             printRect cave (origin, target)
             print $ cave `getRiskLevel` target
             print $ pathToTarget opts cave

pathToTarget :: Options -> Cave -> Maybe (Int, [Node])
pathToTarget Options {..} cave =
  astarSearch (_origin, Torch) isGoalNode nextNodeFn heuristic
  where
    isGoalNode (coord, _) = coord == _target
    heuristic ((x, y), _) =
      let (x', y') = _target
       in (x' - x) ^ 2 + (y' - y) ^ 2
    nextNodeFn ((x, y), currentTool) =
      let neighbourCoords =
            filter
              (\(x', y') -> x' >= 0 && x' <= _maxX && y' >= 0 && y' <= _maxY)
              [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
          neighbours =
            zip neighbourCoords $
            Maybe.mapMaybe (`Map.lookup` cave) neighbourCoords
       in concatMap
            (\(coord', Tile {..}) ->
               let nextToolsAndCosts =
                     if coord' == _target
                       then case currentTool of
                              Torch -> [(Torch, 1)]
                              _     -> [(Torch, 8)]
                       else case (currentTool) of
                              Torch ->
                                case (_tileType) of
                                  Rock   -> [(Torch, 1), (Climb, 8)]
                                  Wet    -> [(Climb, 8), (Neither, 8)]
                                  Narrow -> [(Torch, 1), (Neither, 8)]
                              Climb ->
                                case (_tileType) of
                                  Rock   -> [(Torch, 8), (Climb, 1)]
                                  Wet    -> [(Climb, 1), (Neither, 8)]
                                  Narrow -> [(Torch, 8), (Neither, 8)]
                              Neither ->
                                case (_tileType) of
                                  Rock   -> [(Torch, 8), (Climb, 8)]
                                  Wet    -> [(Climb, 8), (Neither, 1)]
                                  Narrow -> [(Torch, 8), (Neither, 1)]
                in map
                     (\(tool, cost) -> ((coord', tool), cost))
                     nextToolsAndCosts)
            neighbours

-- https://gist.github.com/abhin4v/8172534 Couldn't figure out how to import a
-- hackage package into a script
astarSearch ::
     (Eq a, Hashable a)
  => a
  -> (a -> Bool)
  -> (a -> [(a, Int)])
  -> (a -> Int)
  -> Maybe (Int, [a])
astarSearch startNode isGoalNode nextNodeFn heuristic =
  astar
    (PQ.singleton (heuristic startNode) (startNode, 0))
    Set.empty
    (HashMap.singleton startNode 0)
    HashMap.empty
  where
    astar pq seen gscore tracks
      | PQ.null pq = Nothing
      | isGoalNode node = Just (gcost, findPath tracks node)
      | Set.member node seen = astar pq' seen gscore tracks
      | otherwise = astar pq'' seen' gscore' tracks'
      where
        (node, gcost) = snd . PQ.findMin $ pq
        pq' = PQ.deleteMin pq
        seen' = Set.insert node seen
        successors =
          filter
            (\(s, g, _) ->
               not (Set.member s seen') &&
               (not (s `HashMap.member` gscore) ||
                g < (Maybe.fromJust . HashMap.lookup s $ gscore))) $
          successorsAndCosts node gcost
        pq'' =
          foldl' (\q (s, g, h) -> PQ.insert (g + h) (s, g) q) pq' successors
        gscore' =
          foldl' (\m (s, g, _) -> HashMap.insert s g m) gscore successors
        tracks' =
          foldl' (\m (s, _, _) -> HashMap.insert s node m) tracks successors
    successorsAndCosts node gcost =
      map (\(s, g) -> (s, gcost + g, heuristic s)) . nextNodeFn $ node
    findPath tracks node =
      if HashMap.member node tracks
        then findPath tracks (Maybe.fromJust . HashMap.lookup node $ tracks) ++
             [node]
        else [node]
