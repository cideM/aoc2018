#!/usr/bin/env stack
{-
    stack
    script
    --resolver lts-12.20
    --package containers,trifecta
-}
import qualified Data.List       as List
import qualified Data.Map.Strict as Map
import           Data.Semigroup  (Sum (..), getSum)
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

makeTile :: (Coord, Coord, Depth) -> Cave -> Coord -> Tile
makeTile (origin, target, depth) cave current@(x, y) =
  let geoIndex = getGeoIndex
      erosionLevel = (geoIndex + depth) `mod` 20183
      tileType =
        case erosionLevel `mod` 3 of
          0 -> Rock
          1 -> Wet
          2 -> Narrow
          _ -> error "Wrong erosion level"
   in Tile geoIndex erosionLevel tileType
  where
    getGeoIndex
      | current == origin = 0
      | current == target = 0
      | y == 0 = x * 16807
      | x == 0 = y * 48271
      | otherwise =
        _erosionLevel (cave Map.! (x, y - 1)) *
        _erosionLevel (cave Map.! (x - 1, y))

makeCave :: (Coord, Coord, Depth) -> Cave
makeCave options@(_, target, depth) =
  let coords = concat [[(x, y) | x <- [0 .. (fst target)]] | y <- [0 .. depth]]
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
   in do print (cave Map.! (0, 1))
         print (cave Map.! (1, 0))
         print (cave Map.! (1, 1))
         print coords
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
             let cave = makeCave (origin, target, depth)
             --  printRect cave (origin, target)
             print $ (`getRiskLevel` target) cave
