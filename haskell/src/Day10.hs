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

type DY = Int

type X = Int

type DX = Int

type Position = (X, Y)

type Velocity = (X, Y)

type MaxX = Int

type MaxY = Int

type Time = Int

type Grid = [[Text]]

data Star =
  Star !X
       !Y
       !DX
       !DY
  deriving (Show, Eq, Ord)

-- | Why is this not part of the package? Or am I just too blind to find it?
intP :: Parser Int
intP = read <$> ((:) <$> option ' ' (char '-') <*> some digit)

paramP :: Parser (Int, Int)
paramP = (,) <$> intP <* some (choice [char ',', space]) <*> intP

inputP :: Parser Star
inputP = mkStar <$> (skippedText *> paramP) <* skippedText <*> paramP
  where
    skippedText = some $ choice [space, letter, char '=', char '<', char '>']
    mkStar (px, py) (vx, vy) = Star px py vx vy

-- | Move star t times by its velocity vector
move :: Time -> Star -> Star
move t (Star x y dx dy) = Star (x + t * dx) (y + t * dy) dx dy

-- | Make row of length len and fill with either empty or not empty sign,
-- depending on whether we have a position with an X value corresponding to the
-- current index.
makeRow :: (Text, Text) -> [Star] -> Int -> [Text]
makeRow (emptySign, notEmptySign) stars len = fmap fill [0 .. len]
  where
    fill column =
      let posAtIndex = List.find (\(Star x _ _ _) -> column == x) stars
      in maybe emptySign (const notEmptySign) posAtIndex

-- | (x,y) of bottom right grid point
maxCoords :: [Star] -> (Int, Int)
maxCoords = Foldable.foldr' fn (0, 0)
  where
    fn (Star x y _ _) (maxX, maxY) = (max x maxX, max y maxY)

-- | (x,y) of bottom right grid point
minCoords :: [Star] -> (Int, Int)
minCoords = Foldable.foldr' fn (0, 0)
  where
    fn (Star x y _ _) (minX, minY) = (min x minX, min y minY)

mapWithIndex :: ((a, Int) -> b) -> [a] -> [b]
mapWithIndex f xs = fmap f (zip xs [0 ..])

-- | Find the origin (min x and min y), and then recompute all positions so that
-- the origin is (0,0). E.g., if the origin is (-1,-1) all positions would be
-- re-calculated by x + 1, y + 1
normalize :: [Star] -> [Star]
normalize stars = fmap adjust stars
  where
    (minX, minY) = minCoords stars
    adjust (Star x y dx dy) = Star (x - minX) (y - minY) dx dy

-- | Map from y value to all stars with that y value
posByRow :: [Star] -> Map Y [Star]
posByRow = Foldable.foldr' f Map.empty
  where
    f star@(Star _ y dx dy) = Map.insertWith (++) y [star]

emptyGrid :: Text -> MaxX -> MaxY -> Grid
emptyGrid placeholder x y = replicate (y + 1) (replicate (x + 1) placeholder)

constellations :: [Int] -> [Star] -> [(Int, [Star])]
constellations steps stars =
  (\(step, stars) -> (step, fmap (move step) stars)) <$>
  zip steps (repeat stars)

makeGrid :: [Star] -> Grid
makeGrid stars = mapWithIndex fillRow $ emptyGrid "." x y
  where
    (x, y) = maxCoords stars
    yMap = posByRow stars
    makeRow' = makeRow (".", "o")
    fillRow (row, y') =
      let stars' = Map.lookup y' yMap
      in maybe row (`makeRow'` x) stars'

-- | Not really size of a grid but rather how far apart the top left and bottom
-- right points are.
size :: [Star] -> Int
size stars =
  let (maxX, maxY) = maxCoords stars
      (minX, minY) = minCoords stars
  in (maxX - minX) + (maxY - minY)

showGrid :: Grid -> Text
showGrid = Text.unlines . fmap Text.unwords

run :: Text -> Either ErrMsg Text
run t =
  case traverse (parse . Text.unpack) $ Text.lines t of
    Failure _ -> Left "Parsing failed"
    Success stars ->
      Right $ Text.pack (show t') `Text.append` "\n" `Text.append` grid
      where comparator (_, stars1) (_, stars2) =
              compare (size stars1) (size stars2)
            (t', stars') =
              List.minimumBy comparator $ constellations [0 .. 15000] stars
            grid = showGrid $ makeGrid (normalize stars')
  where
    parse = parseString inputP mempty

prog :: DayProg
prog = DayProg "day9" run
