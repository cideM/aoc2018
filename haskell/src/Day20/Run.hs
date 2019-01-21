{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Day20.Run
  ( run
  ) where

import           Control.Monad.State
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import qualified Data.Maybe          as Maybe
import           Data.Sequence       (Seq)
import qualified Data.Sequence       as Seq
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Text.Printf
import           Types

{-
I copied this from
https://www.reddit.com/r/adventofcode/comments/a7uk3f/2018_day_20_solutions/ec7cdtk

I do understand how it works. I don't understand how I can come up with code
like that from the start.

My solution was overcomplicated. It solved the test cases but ran seemingly
forever on the actual data because it was about as inefficient as it gets and
was just creating thunks. I'm so fucking frustrated right now. :( I hate
Haskell but I also don't want to be one of the "stupid programmers" who can't
handle it. And many solutions out there solve the problem with so few lines of
code. I want to be able to do that too :(
-}
type Coords = (Int, Int)

type Doors = Map (Int, Int) [(Int, Int)]

parse :: [Coords] -> Coords -> String -> State Doors ()
parse prev location@(x, y) (current:remaining) =
  case current of
    'N' -> connect location (x, y - 1) >>= \new -> parse prev new remaining
    'E' -> connect location (x + 1, y) >>= \new -> parse prev new remaining
    'W' -> connect location (x - 1, y) >>= \new -> parse prev new remaining
    'S' -> connect location (x, y + 1) >>= \new -> parse prev new remaining
    '(' -> parse (location : prev) location remaining
    ')' -> parse (tail prev) location remaining
    '|' -> parse prev (head prev) remaining
    '$' -> pure ()
    _   -> parse prev location remaining

connect :: Coords -> Coords -> State Doors Coords
connect a b = do
  modify $ Map.insertWith mappend a [b]
  modify $ Map.insertWith mappend b [a]
  return b

bfs :: Seq (Coords, Int) -> Map Coords Int -> Doors -> [Int]
bfs Seq.Empty visited _ = Map.elems visited
bfs ((coords, len) Seq.:<| queue) visited cnxs
  | coords `Map.member` visited = bfs queue visited cnxs
  | otherwise =
    let next = Maybe.fromMaybe [] $ coords `Map.lookup` cnxs
        next' = Seq.fromList $ (, len + 1) <$> next
     in bfs (queue Seq.>< next') (Map.insert coords len visited) cnxs

run :: Text -> Either ErrMsg Text
run input =
  let parsed = flip execState Map.empty $ parse [] (0, 0) (Text.unpack input)
      paths = bfs (Seq.singleton ((0, 0), 0)) Map.empty parsed
   in Right . Text.pack $
      "p1: " ++
      show (maximum paths) ++
      " p2: " ++ show (length . filter (>= 1000) $ paths)
