{-# LANGUAGE OverloadedStrings #-}

module Day2
  ( prog
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

-- | numDuplicateChars returns the number of sets of duplicate characters of
-- length num
-- numDuplicateChars 2 "aabbcc" -> 3
numDuplicateChars :: Int -> Text -> Int
numDuplicateChars num =
  List.length
    . List.filter ((==) num . List.length)
    . List.group
    . List.sort
    . Text.unpack

-- | checkSum returns a tuple where the 1st element is 1 if the string has
-- exactly two same characters and the 2nd element if the string has three.
-- checkSum [1,1,2] -> (1, 0)
checkSum :: Text -> (Int, Int)
checkSum t =
  let twos   = if numDuplicateChars 2 t > 0 then 1 else 0
      threes = if numDuplicateChars 3 t > 0 then 1 else 0
  in  (twos, threes)

-- | Compares two lists and returns the common elements if the lists differ by
-- exactly 1 element. Otherwise returns an empty list.
diffBy1 :: (Ord a, Eq a) => [a] -> [a] -> [a]
diffBy1 xs ys =
  let common'   = common xs ys
      shortest = List.foldr min xs [ys]
      diff     = (List.length shortest) - (List.length common')
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
common xs ys = fmap fst . List.filter (\(a, b) -> a == b) $ List.zip xs ys

run :: Text -> Either ErrMsg Showable
run t =
  let ls        = Text.lines t
      checkSums = checkSum <$> ls
      sumFst    = sum $ fmap fst checkSums
      sumSnd    = sum $ fmap snd checkSums
      common    = compareElems diffBy1 $ fmap Text.unpack ls
  in  Right . Types.pack $ Day2Result (sumFst * sumSnd) $ Text.pack common

prog :: DayProg
prog = DayProg "day2" run
