#!/usr/bin/env stack
{-
    stack
    script
    --resolver lts-12.20
    --package containers,text
    --
    -Wall
    -Werror
-}
{-# LANGUAGE OverloadedStrings #-}

module Day20 where

import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.Semigroup     (Max (..))
import qualified Data.Semigroup     as Semigroup
import           Data.Sequence      (Seq (..), (|>))
import qualified Data.Sequence      as Seq
import qualified Data.Text          as Text
import qualified Data.Text.IO       as TIO
import qualified System.Environment as Env

type Coords = (Int, Int)

data Direction
  = N
  | W
  | S
  | E
  deriving (Show, Eq)

buildMap ::
     Coords -- ^ Origin
  -> String -- ^ String we're parsing
  -> Map Coords Int -- ^ Map from point to its distance from origin
buildMap origin stringToParse =
  let initialStack = Seq.singleton (origin, 0)
      initialMap = Map.fromList [(origin, 0)]
   in go initialStack stringToParse initialMap
  where
    go _ [] map' = map'
    go stack (c:cs) map' =
      let (stack' :|> curPoint) = stack
          nextPoint' = nextPoint curPoint
       in case c of
            'N' ->
              let s = nextPoint' N
               in go' (stack' |> s) (insert s)
            'E' ->
              let s = nextPoint' E
               in go' (stack' |> s) (insert s)
            'W' ->
              let s = nextPoint' W
               in go' (stack' |> s) (insert s)
            'S' ->
              let s = nextPoint' S
               in go' (stack' |> s) (insert s)
            '(' -> go' (stack |> curPoint) map'
            ')' -> go' stack' map'
            '|' ->
              let (_ Seq.:|> last') = stack'
               in go' (stack' |> last') map'
            '$' -> map'
            _ -> go' stack map'
      where
        insert =
          let f k v = Map.insertWith (flip const) k v map'
           in uncurry f
        go' nextStack = go nextStack cs
        nextPoint ((x, y), dist) N = ((x, y - 1), dist + 1)
        nextPoint ((x, y), dist) S = ((x, y + 1), dist + 1)
        nextPoint ((x, y), dist) W = ((x - 1, y), dist + 1)
        nextPoint ((x, y), dist) E = ((x + 1, y), dist + 1)

maxDist :: Map Coords Int -> Int
maxDist = Semigroup.getMax . foldMap Max . Map.elems

over1000 :: Map Coords Int -> Int
over1000 = length . filter (>= 1000) . Map.elems

main :: IO ()
main = do
  args <- Env.getArgs
  input <- TIO.readFile (head args)
  let map' = buildMap (0, 0) (Text.unpack input)
  TIO.putStrLn . Text.pack $ "p1: " ++ show (maxDist map')
  TIO.putStrLn . Text.pack $ "p2: " ++ show (over1000 map')
