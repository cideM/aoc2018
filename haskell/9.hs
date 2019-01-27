#!/usr/bin/env stack
{-
    stack
    script
    --resolver lts-12.20
    --package trifecta,containers
-}
{-# LANGUAGE BangPatterns #-}

module Day9 where

import qualified Data.Foldable      as Foldable
import qualified Data.IntMap.Strict as Map
import           Data.Sequence      (Seq ((:<|)), (<|), (><))
import qualified Data.Sequence      as Seq
import           Text.Trifecta

main :: IO ()
main = do
  input <- parseString inputP mempty <$> getContents
  mapM_ (print . uncurry run) input
  mapM_ (print . uncurry run . fmap (* 100)) input

getNextPlayer :: Int -> Int -> Int
getNextPlayer maxPlayers !current -- ^ Remove bang and observe stack overflow ~_~
  | current >= maxPlayers = 1
  | otherwise = current + 1

run :: Int -> Int -> Int
run playerCount lastMarble = go (Seq.singleton 0) Map.empty 0 1
  where
    go ring scores currentPlayer marble
      | marble > lastMarble = maximum $ Map.elems scores
      | marble `mod` 23 == 0 =
        let (xs, remove :<| ys) = Seq.splitAt (Foldable.length ring - 7) ring
            newScores =
              Map.insertWith (+) currentPlayer (marble + remove) scores
         in go (ys >< xs) newScores (getNextPlayer' currentPlayer) (marble + 1)
      | Foldable.length ring == 1 =
        go (marble <| ring) scores (getNextPlayer' currentPlayer) (marble + 1)
      | otherwise =
        let (l, r) = Seq.splitAt 2 ring
         in go
              ((marble <| r) >< l)
              scores
              (getNextPlayer' currentPlayer)
              (marble + 1)
    getNextPlayer' = getNextPlayer playerCount

-- | Player count and last marble value
inputP :: Parser (Int, Int)
inputP =
  (,) <$> (fromIntegral <$> integer) <*>
  (many (choice [space, char ';', letter]) *> (fromIntegral <$> integer))
