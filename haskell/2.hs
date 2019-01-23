#!/usr/bin/env stack
{-
    stack
    script
    --resolver lts-12.20
    --package containers
-}
import           Control.Arrow ((&&&))
import           Control.Monad (guard)
import qualified Data.Foldable as Foldable
import qualified Data.List     as List
import           Data.Map      (Map)
import qualified Data.Map      as Map
import qualified Data.Maybe    as Maybe

diffBy1 :: (Eq a) => [a] -> [a] -> Maybe ([a], [a])
diffBy1 as bs = do
  guard $ length as == length bs
  let equals = filter (uncurry (/=)) $ zip as bs
  guard $ length equals == 1
  return (as, bs)

pairs :: [a] -> [(a, a)]
pairs xs = do
  x:ys <- List.tails xs
  y <- ys
  return (x, y)

occurrences :: (Eq a, Ord a) => [a] -> Map a Integer
occurrences = Map.fromListWith (+) . flip zip [1,1 ..]

sameItems :: (Eq a) => [a] -> [a] -> [a]
sameItems as bs = map fst . filter (uncurry (==)) $ zip as bs

main :: IO ()
main = do
  input <- getContents
  let lines' = lines input
  -- | Part 1
  print . -- ^ Profit
    uncurry (*) .
    -- ^ make (*) take a tuple instead of two separate args
    (count 2 &&& count 3) .
    -- ^ Arrows... ultimate readability killers. But here we take the input,
    -- run it through both functions and get a tuple as a result, e.g., (1,2)
    map (Map.elems . occurrences) $
    -- ^ Each input line is transformed into a map of char occurrences. Then we
    -- just take the occurrences, e.g. [1,1,2,3]
    lines'
  -- | Part 2
  mapM_ (print . uncurry sameItems) . List.find (Maybe.isJust . uncurry diffBy1) $
    pairs lines'
  where
    count n xs = Foldable.length $ filter (elem n) xs
