{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Day11 where

import qualified Data.Array.Unboxed as UArray
import Data.Array.Unboxed (UArray, (!))
import qualified Data.Foldable as Foldable
import qualified Data.List.Extra as Extra
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Vector.Unboxed as UVector
import Text.Printf
import Types

type Coords = (Int, Int)

type Power = Int

type BoxSize = Int

type Grid = UArray (Int, Int) Int

plevel :: Int -> (Int, Int) -> Int
plevel serialNum (x, y) =
    let rackID = x + 10
     in (((rackID * y + serialNum) * rackID) `div` 100 `mod` 10) - 5

mkGrid :: Int -> Int -> Grid
mkGrid size serialNum =
    UArray.array ((1, 1), (size, size)) $ do
        y <- [1 .. size]
        x <- [1 .. size]
        return ((x, y), plevel serialNum (x, y))

printGrid :: Grid -> IO ()
printGrid g =
    let gridSize = uncurry max . snd $ UArray.bounds g
     in mapM_ (putStrLn . unwords) .
        map (map $ Text.Printf.printf "%+d") . Extra.chunksOf gridSize $
        UArray.elems g

run :: Text -> Either ErrMsg Text
run t =
    let g = mkGrid 300 . read $ Text.unpack t
        p1 = part1 g
        p2 = part2 g
        p1Result = Extra.maximumOn fst3 p1
     in Right . Text.pack $ show p1Result ++ " " ++ show p2

part1 :: Grid -> [(Int, Int, Int)]
part1 g = do
    x <- [1 .. maxX - 2]
    y <- [1 .. maxY - 2]
    let total =
            foldl (\accum (x', y') -> accum + (g ! (x + x', y + y'))) 0 $ do
                x' <- [0 .. 2]
                y' <- [0 .. 2]
                return (x', y')
    return (total, x, y)
  where
    maxX = fst . snd $ UArray.bounds g
    maxY = snd . snd $ UArray.bounds g

trd :: (a, a, a) -> a
trd (_, _, x) = x

boxes :: Grid -> Int -> Int -> UVector.Vector (BoxSize, Power)
boxes g x y =
    let gridSize = uncurry max . snd $ UArray.bounds g
        maxBoxSize x' y' = gridSize - max x' y'
     in snd . Foldable.foldl' f (0, UVector.empty) $ do
            s <- [0 .. maxBoxSize x y]
            let temp = do
                    a <- [0 .. s - 1]
                    return $ g ! (x + s, y + a) + g ! (x + a, y + s)
            return (s, g ! (x + s, y + s) + (sum temp))
  where
    f (p', acc) (bsize, p) =
        let pow' = p + p'
         in (pow', UVector.snoc acc (bsize, pow'))

fst3 :: (a, b, a) -> a
fst3 (x, _, _) = x

part2 :: Grid -> (Int, Coords, Int) -- ^ Power, Key, Grid
part2 g =
    Foldable.maximumBy (\x1 x2 -> fst3 x1 `compare` fst3 x2) $ do
        x <- [1 .. maxX]
        y <- [1 .. maxY]
        let (size, total) = UVector.maximumBy (\x1 x2 -> snd x1 `compare` snd x2) $ boxes g x y
        return (total, (x, y), size + 1)
  where
    maxX = fst . snd $ UArray.bounds g
    maxY = snd . snd $ UArray.bounds g

prog :: DayProg
prog = DayProg "day11" run
