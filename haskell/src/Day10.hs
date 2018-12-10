{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

module Day10 where

import Control.Applicative ((<|>))
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq((:<|)), Seq, (<|), (><), (|>))
import qualified Data.Text as Text
import Data.Text (Text)
import Text.Parser.Char
import Text.Trifecta
import Types

newtype Position =
    Position (Int, Int)
    deriving (Show)

newtype Velocity =
    Velocity (Int, Int)
    deriving (Show)

-- | Why is this not part of the package? Or am I just too blind to find it?
intP :: Parser Int
intP = read <$> ((:) <$> option ' ' (char '-') <*> some digit)

paramP :: Parser (Int, Int)
paramP = (,) <$> intP <* some (choice [char ',', space]) <*> intP

inputP :: Parser (Position, Velocity)
inputP =
    (,) <$> (skippedText *> (Position <$> paramP)) <* skippedText <*>
    (Velocity <$> paramP)
  where
    skippedText = some $ choice [space, letter, char '=', char '<', char '>']

move :: Time -> Position -> Velocity -> Position
move t (Position (x, y)) (Velocity (dx, dy)) =
    Position $ (,) (x + t * dx) (y + t * dx)

type Time = Int

maxCoords :: [Position] -> (Int, Int)
maxCoords = Foldable.foldr' fn (0, 0)
  where
    fn (Position (x, y)) (maxX, maxY) = (max x maxX, max y maxY)

type MaxX = Int

type MaxY = Int

newtype Grid a =
    Grid [[a]]
    deriving (Show)

posToGrid :: Grid a -> [Position] -> Grid a
posToGrid grid pos = fmap fn . zip grid $ List.groupBy pos
    where
        groupByRow (Position (_,y)) (Position (_,y2)) = y == y2
        fn (Position (x,y), row) = let (l, _:r) = splitAt x row
                                   in 

makeGrid :: (Show a) => a -> MaxX -> MaxY -> Grid a
makeGrid val x y = Grid $ replicate y (replicate x val)

posGrid :: Time -> [(Position, Velocity)] -> Grid Text
posGrid t xs = emptyGrid
  where
    moved = fmap (uncurry $ move t) xs
    makeGrid' = uncurry $ makeGrid "#"
    emptyGrid = makeGrid' $ maxCoords moved

run :: Text -> Either ErrMsg Text
run t =
    case traverse (parse . Text.unpack) $ Text.lines t of
        Failure _ -> Left "Parsing failed"
        Success xs ->
            let (Grid rows) = posGrid 0 xs
             in Right . Text.unlines $ Text.unwords <$> rows
  where
    parse = parseString inputP mempty

prog :: DayProg
prog = DayProg "day9" run
