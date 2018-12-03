{-# LANGUAGE OverloadedStrings #-}

module Day2
  ( prog, compareElems
  )
where

import           Data.Text                     as Text
import qualified Data.List                     as List
import           Types
import           Data.Text.Read                as Read
import           Data.Set                       ( Set )
import qualified Data.Traversable              as Traversable
import qualified Data.Foldable                 as Foldable
import qualified Data.Set                      as Set

type Checksum = Int

type CommonChars = Text

data Day2Result = Day2Result !Checksum !CommonChars deriving (Show)

type Twos = Int

type Threes = Int

-- | similarSets returns a list of integers. Each integer represents the length of
-- a set of similar elements. Those lengths are unique, meaning if a list
-- contains n sets of similar elements, they're all only counted once.
-- In other words, similarSets tells you if a list has similar sets of n
-- characters, not how many.
--
-- similarSets [4,4,3,5,5]
-- -> [
--   1,
--   ^ One set of length 1 containing the element (3)
--   2
--   ^ Two sets of length 2, containing 4 and 5 respectively. Only counted
--   once.
-- ]
similarSets :: Ord a => [a] -> [Int]
similarSets = List.nub . fmap List.length . List.group . List.sort

-- | Compares two lists and returns the common elements if the lists differ by
-- exactly 1 element. Otherwise returns an empty list.
diffBy1 :: (Ord a, Eq a) => [a] -> [a] -> [a]
diffBy1 xs ys =
  let common'  = common xs ys
      shortest = List.foldr min xs [ys]
      diff     = List.length shortest - List.length common'
  in  if diff == 1 then common' else []

-- | compareElems takes a comparator function, which is called with every
-- combination of elements in the list.
compareElems :: Eq a => ([a] -> [a] -> [a]) -> [[a]] -> [a]
compareElems f xs = go xs []
 where
  go []        out = List.concat out
  go (x : xs') out = go xs' $ out ++ fmap (f x) xs'

-- | common returns the list elements that are in the same position in
-- both lists
common :: Eq a => [a] -> [a] -> [a]
common xs ys = fmap fst . List.filter (uncurry (==)) $ List.zip xs ys

run :: Text -> Either ErrMsg Text
run t = Right . Text.pack . show $ Day2Result (twos * threes) $ Text.pack common
 where
  ls     = Text.unpack <$> Text.lines t
  common = compareElems diffBy1 ls
  -- We take the list of lists (newline delimited strings) and compare each
  -- element with all other elements using diffBy1 as comparator.
  foldFn xs (x, y)  =
    let twos   = List.length $ List.filter (2 ==) xs
        -- ^ Get the number of occurences of 2 resp. 3 (below) in the list
        threes = List.length $ List.filter (3 ==) xs
    in  (x + twos, y + threes)
        -- ^ Now we have the number of 2 and 3 element 
  (twos, threes) = List.foldr foldFn (0, 0) $ fmap similarSets ls
  -- ^ Mapping similarSets over our list of texts gives us the groups of
  -- similar characters of length n. Since similarSets applies List.nub to its
  -- output, we only get a single entry for each n similar characters.

prog :: DayProg
prog = DayProg "day2" run
