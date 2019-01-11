{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Day13 where

import           Data.Foldable   (foldl')
import           Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import qualified Data.Maybe      as Maybe
import           Data.Text       (Text)
import qualified Data.Text       as Text
import qualified Data.Text.IO    as TextIO
import           Debug.Trace
import           Types

data Coords =
  Coords Int
         Int
  deriving (Eq, Ord, Show)

data Direction
  = Horizontal
  | Vertical
  deriving (Show, Eq)

data Track
  = Intersection
  | Straight Direction
  | Corner (Int, Int)
  deriving (Show, Eq)

data IntersectionDirection
  = Left'
  | Straight'
  | Right'
  deriving (Show)

data Cart = Cart
  { velocityVector   :: (Int, Int)
  , nextIntersection :: IntersectionDirection
  } deriving (Show)

newVectorForCorner :: Coords -> Char -> Map Coords Track -> (Int, Int)
newVectorForCorner (Coords x y) cornerSymbol tracks
  | maybe
     False
     (\track -> track == Straight Horizontal || track == Intersection)
     (Map.lookup (Coords (x - 1) y) tracks) =
    if cornerSymbol == '/'
      then (-1, -1) -- South east
      else (-1, 1) -- North east
  | otherwise =
    if cornerSymbol == '/'
      then (1, 1) -- North west
      else (1, -1) -- South west

parseTracks :: Text -> (Map Coords Track, Map Coords Cart)
parseTracks input = foldl' f (Map.empty, Map.empty) lines'
  where
    lines' = zip [0 ..] . Prelude.lines $ Text.unpack input
    f acc (row, line) =
      foldl'
        (\(tracks, carts) (col, char) ->
           let insert = Map.insert (Coords col row)
               makeCart velocity =
                 Cart {velocityVector = velocity, nextIntersection = Left'}
            in case char of
                 '-' -> (insert (Straight Horizontal) tracks, carts)
                 '|' -> (insert (Straight Vertical) tracks, carts)
                 '+' -> (insert Intersection tracks, carts)
                 '/' ->
                   ( insert
                       (Corner (newVectorForCorner (Coords col row) char tracks))
                       tracks
                   , carts)
                 '\\' ->
                   ( insert
                       (Corner (newVectorForCorner (Coords col row) char tracks))
                       tracks
                   , carts)
                 '^' ->
                   ( insert (Straight Vertical) tracks
                   , insert (makeCart (0, -1)) carts)
                 'v' ->
                   ( insert (Straight Vertical) tracks
                   , insert (makeCart (0, 1)) carts)
                 '>' ->
                   ( insert (Straight Horizontal) tracks
                   , insert (makeCart (1, 0)) carts)
                 '<' ->
                   ( insert (Straight Horizontal) tracks
                   , insert (makeCart (-1, 0)) carts)
                 _ -> (tracks, carts))
        acc
        (zip [0 ..] line)

-- Debugging
testData = TextIO.readFile "../data/day13_test.txt"

-- Debugging
advanceBy ::
     Int -> Map Coords Cart -> Map Coords Track -> (Map Coords Cart, [Coords])
advanceBy i carts tracks = go carts i
  where
    go cs 0 = advanceCarts cs tracks
    go cs i' =
      let (carts', _) = advanceCarts cs tracks
       in go carts' (i' - 1)

findCrash :: Map Coords Cart -> Map Coords Track -> Coords
findCrash carts tracks = go carts []
  where
    go _ [x] = x
    go carts' _ =
      let (carts'', collisions) = advanceCarts carts' tracks
       in go carts'' collisions

findLastCart :: Map Coords Cart -> Map Coords Track -> Coords
findLastCart carts tracks
  | Map.size carts == 1 = fst . Prelude.head $ Map.toList carts
  | otherwise = findLastCart (fst $ advanceCarts carts tracks) tracks

newCartForIntersection :: Cart -> Cart
newCartForIntersection Cart {nextIntersection = Left', velocityVector} =
  case velocityVector of
    (1, 0)  -> Cart (0, -1) Straight'
    (0, -1) -> Cart (-1, 0) Straight'
    (-1, 0) -> Cart (0, 1) Straight'
    (0, 1)  -> Cart (1, 0) Straight'
newCartForIntersection Cart {nextIntersection = Right', velocityVector} =
  case velocityVector of
    (1, 0)  -> Cart (0, 1) Left'
    (0, -1) -> Cart (1, 0) Left'
    (-1, 0) -> Cart (0, -1) Left'
    (0, 1)  -> Cart (-1, 0) Left'
newCartForIntersection cart@Cart {nextIntersection = Straight'} =
  cart {nextIntersection = Right'}

advanceCarts ::
     Map Coords Cart -> Map Coords Track -> (Map Coords Cart, [Coords])
advanceCarts carts tracks = Map.foldlWithKey f (carts, []) carts
  where
    f (acc, collisions) coords@(Coords x y) c@Cart {..} =
      let dx = fst velocityVector
          dy = snd velocityVector
          currentTrackSegment = tracks ! coords
          (nextCoords, nextCar) =
            case currentTrackSegment of
              Intersection ->
                let cart'@Cart {velocityVector = velocityVector'} =
                      newCartForIntersection c
                    (dx', dy') = velocityVector'
                 in (Coords (x + dx') (y + dy'), cart')
              Corner (dx', dy') ->
                let (dx'', dy'') = (dx + dx', dy + dy')
                    newCoords = Coords (x + dx'') (y + dy'')
                 in (newCoords, (c {velocityVector = (dx'', dy'')}))
              _ -> (Coords (x + dx) (y + dy), c)
          withoutCurrent = Map.delete coords acc
          wasPartOfCollision = coords `elem` collisions
          hasCollision = Maybe.isJust (Map.lookup nextCoords withoutCurrent)
       in if hasCollision
            then (Map.delete nextCoords withoutCurrent, nextCoords : collisions)
               -- somehow need to delete the deleted in its iteration
            else if wasPartOfCollision
                   then (withoutCurrent, collisions)
                   else ( Map.insert nextCoords nextCar withoutCurrent
                        , collisions)

run :: Text -> Either ErrMsg Text
run t =
  Right . Text.pack $ show (findCrash carts tracks) ++ " part2: " ++ show part2
  where
    (tracks, carts) = parseTracks t
    part2 = findLastCart carts tracks

prog :: DayProg
prog = DayProg "day13" run
