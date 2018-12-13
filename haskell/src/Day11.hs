{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Day11 where

import qualified Data.List.Extra as Extra
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Vector as Vector
import Data.Vector (Vector, (!?))
import Debug.Trace
import Types

type Coords = (Int, Int)

newtype Power =
    Power Int
    deriving (Ord, Eq, Show)

newtype BoxSize =
    BoxSize Int
    deriving (Ord, Eq, Show)

data Cell = Cell
    { x :: !Int
    , y :: !Int
    , power :: !Int
    } deriving (Show, Eq, Ord)

data Grid = Grid
    { size :: !Int
    , cells :: Vector Cell
    } deriving (Show, Eq)

plevel :: Int -> (Int, Int) -> Int
plevel serialNum (x, y) =
    let rackID = x + 10
     in (((rackID * y + serialNum) * rackID) `div` 100 `mod` 10) - 5

mkGrid :: Int -> Int -> Grid
mkGrid size serialNum =
    Grid size . Vector.fromList $ do
        y <- [1 .. size]
        x <- [1 .. size]
        let p = plevel serialNum (x, y)
        return $ Cell x y p

run :: Text -> Either ErrMsg Text
run t =
    let g = mkGrid 300 . read $ Text.unpack t
        p1 = part1 g
        p2 = part2 g
        p1Result = Extra.maximumOn fst3 p1
        p2Result = Extra.maximumOn fst3 p2
     in Right . Text.pack $ show p1Result ++ " " ++ show p2Result

getCell :: Grid -> Int -> Int -> Maybe Cell
getCell Grid {..} x y = cells !? ((x - 1) + (y - 1) * size)

part1 :: Grid -> [(Int, Int, Int)]
part1 g@Grid {..} = do
    x <- [1 .. size]
    y <- [1 .. size]
    let total =
            foldr
                (\(x', y') accum ->
                     maybe
                         accum
                         ((+) accum . power)
                         (getCell' (x + x') (y + y')))
                0 $ do
                x' <- [0 .. 2]
                y' <- [0 .. 2]
                return (x', y')
    return (total, x, y)
  where
    getCell' = getCell g

trd :: (a, a, a) -> a
trd (_, _, x) = x

boxes :: Grid -> Int -> Int -> Vector (BoxSize, Vector Coords)
boxes grid x y =
    let maxBoxSize x' y' = (size grid) - max x' y'
     in Vector.fromList $ do
            s <- [1 .. maxBoxSize x y]
            let temp = do
                    a <- [0 .. s - 1]
                    return [(x + s, y + a), (x + a, y + s)]
            return (BoxSize s, Vector.fromList $ (x + s, y + s) : concat temp)

fst3 :: (a, b, a) -> a
fst3 (x, _, _) = x

-- I'm pretty sure this is broken right now. An older version works (check git
-- log) but I wanted to improve performance :(
part2 :: Grid -> [(Int, Coords, Int)] -- ^ Power, Key, Grid
part2 g@Grid {size = gridSize} = do
    x <- [1 .. gridSize - 1]
    y <- [1 .. gridSize - 1]
    let (BoxSize size, Power total) =
            Extra.maximumOn snd . fst $
            Vector.foldl' (sumBox g) ([], getOrZero' x y) $ boxes g x y
    return (total, (x, y), size)
  where
    sumBox g (results, accumSum) (BoxSize size, coords) =
        let power =
                Vector.foldr'
                    (\(x,y) s -> s + getOrZero' x y)
                    0
                    coords
         in ((BoxSize size, Power power) : results, accumSum + power)
    getOrZero' x y = maybe 0 power $ getCell g x y

prog :: DayProg
prog = DayProg "day11" run
