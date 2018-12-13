{-# LANGUAGE OverloadedStrings #-}

module Day11Alt where

import qualified Data.List.Extra as Extra
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Printf as Printf
import Types

type Coords = (Int, Int)

newtype Power =
  Power Int
  deriving (Ord, Eq, Show)

newtype BoxSize =
  BoxSize Int
  deriving (Ord, Eq, Show)

key :: Coords -> String
key (i, ii) = Printf.printf "%05d%05d" i ii

plevel :: Int -> Int -> Int -> Int
plevel serialNum x y =
  let rackID = x + 10
  in (((rackID * y + serialNum) * rackID) `div` 100 `mod` 10) - 5

mkMap :: Int -> Int -> Int -> Map String Int
mkMap x y serialNum =
  M.fromList $ do
    x' <- [1 .. x]
    y' <- [1 .. y]
    return (key (x', y'), plevel serialNum x' y')

run :: Text -> Either ErrMsg Text
run t =
  let m = mkMap 300 300 . read $ T.unpack t
      p1 = part1 298 298 m
      p2 = part2 300 300 m
      p1Result = Extra.maximumOn fst p1
      p2Result = Extra.maximumOn fst3 p2
  in Right . T.pack $ show p1Result ++ " " ++ show p2Result

part1 :: Int -> Int -> Map String Int -> [(Int, String)]
part1 maxX maxY m = do
  x <- [1 .. maxX]
  y <- [1 .. maxY]
  let total =
        foldr
          (\(x', y') sum ->
             let key' = key (x + x', y + y')
             in maybe sum (sum +) (M.lookup key' m))
          0 $ do
          x' <- [0 .. 2]
          y' <- [0 .. 2]
          return (x', y')
  return (total, key (x, y))

boxes :: Int -> Int -> Int -> [(BoxSize, [Coords])]
boxes gridSize x y =
  let maxBoxSize x y = gridSize - max x y
  in do s <- [1 .. maxBoxSize x y]
        let temp = do
              a <- [0 .. s - 1]
              return [(x + s, y + a), (x + a, y + s)]
        return (BoxSize s, (x + s, y + s) : concat temp)

getOrZero :: Map String Int -> Coords -> Int
getOrZero m coordPair = Maybe.fromMaybe 0 (M.lookup (key coordPair) m)

fst3 (x, _, _) = x

part2 :: Int -> Int -> Map String Int -> [(Int, String, Int)] -- ^ Power, Key, Grid
part2 x y m = do
  x <- [1 .. x]
  y <- [1 .. y]
  let (BoxSize size, Power sum) =
        Extra.maximumOn snd . fst $
        foldl sumBox ([], getOrZero' (x, y)) $ boxes 300 x y
  return (sum, key (x, y), size)
  where
    getOrZero' = getOrZero m
    sumBox ::
         ([(BoxSize, Power)], Int)
      -> (BoxSize, [Coords])
      -> ([(BoxSize, Power)], Int)
    sumBox (results, accumSum) (BoxSize size, coords) =
      let power = sum $ map getOrZero' coords
      in ((BoxSize size, Power power) : results, accumSum + power)

prog :: DayProg
prog = DayProg "day11" run
