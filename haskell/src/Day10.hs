{-# LANGUAGE OverloadedStrings #-}

module Day10 where

import Control.Applicative ((<|>))
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import Data.Sequence (Seq((:<|)), Seq, (<|), (><), (|>))
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Debug.Trace as Trace
import Text.Parser.Char
import Text.Trifecta
import Types

type Y = Int

type X = Int

type Position = (X, Y)

type Velocity = (X, Y)

type MaxX = Int

type MaxY = Int

type Time = Int

type Grid = [[Text]]

-- | Why is this not part of the package? Or am I just too blind to find it?
intP :: Parser Int
intP = read <$> ((:) <$> option ' ' (char '-') <*> some digit)

paramP :: Parser (Int, Int)
paramP = (,) <$> intP <* some (choice [char ',', space]) <*> intP

inputP :: Parser (Position, Velocity)
inputP = (,) <$> (skippedText *> paramP) <* skippedText <*> paramP
  where
    skippedText = some $ choice [space, letter, char '=', char '<', char '>']

-- | Move position t times by its velocity vector
move :: Time -> (Position, Velocity) -> Position
move t ((x, y), (dx, dy)) = (,) (x + t * dx) (y + t * dy)

-- | Make row of length len and fill with either empty or not empty sign,
-- depending on whether we have a position with an X value corresponding to the
-- current index.
makeRow :: (Text, Text) -> [Position] -> Int -> [Text]
makeRow (emptySign, notEmptySign) pos len = fmap fill [0 .. len]
  where
    fill i =
      let posAtIndex = List.find ((==) i . fst) pos
      in maybe emptySign (const notEmptySign) posAtIndex

-- | (x,y) of bottom right grid point
maxCoords :: [Position] -> (Int, Int)
maxCoords = Foldable.foldr' fn (0, 0)
  where
    fn (x, y) (maxX, maxY) = (max x maxX, max y maxY)

-- | (x,y) of bottom right grid point
minCoords :: [Position] -> (Int, Int)
minCoords = Foldable.foldr' fn (0, 0)
  where
    fn (x, y) (minX, minY) = (min x minX, min y minY)

mapWithIndex :: ((a, Int) -> b) -> [a] -> [b]
mapWithIndex f xs = fmap f (zip xs [0 ..])

-- | Find the origin (min x and min y), and then recompute all positions so that
-- the origin is (0,0). E.g., if the origin is (-1,-1) all positions would be
-- re-calculated by x + 1, y + 1
normalize :: [Position] -> [Position]
normalize ps = fmap adjust ps
  where
    (minX, minY) = minCoords ps
    adjust (x, y) = (x - minX, y - minY)

-- | Map from y value to all positions with that y value
posByRow :: [Position] -> Map Y [Position]
posByRow = Foldable.foldr' f Map.empty
  where
    f p@(_, y) = Map.insertWith (++) y [p]

emptyGrid :: Text -> MaxX -> MaxY -> Grid
emptyGrid placeholder x y = replicate (y + 1) (replicate (x + 1) placeholder)

makeGrid :: MaxX -> MaxY -> [Position] -> Grid
makeGrid x y pos = mapWithIndex fillRow $ emptyGrid "." x y
  where
    yMap = posByRow pos
    makeRow' = makeRow (".", "o")
    fillRow (row, y') =
      let pos' = Map.lookup y' yMap
      in maybe row (`makeRow'` x) pos'

-- | Not really size of a grid but rather how far apart the top left and bottom
-- right points are.
size :: [Position] -> Int
size pos =
  let (maxX, maxY) = maxCoords pos
      (minX, minY) = minCoords pos
  in (maxX - minX) + (maxY - minY)

run :: Text -> Either ErrMsg Text
run t =
  case traverse (parse . Text.unpack) $ Text.lines t of
    Failure _ -> Left "Parsing failed"
    -- | xs = [(Position, Velocity)]
    Success xs ->
      let closestPositions
            -- | Simulate the positions after different time spans, find the one
            -- where they are closest together and turn that into a grid
           =
            let attempts = fmap (\t -> fmap (move t) xs) [0 .. 50000]
            in List.minimumBy (\x y -> compare (size x) (size y)) attempts
          normalized = normalize closestPositions
          (maxX, maxY) = maxCoords normalized
          grid = Text.unlines $ Text.unwords <$> makeGrid maxX maxY normalized
      in Right grid
  where
    parse = parseString inputP mempty

prog :: DayProg
prog = DayProg "day9" run
