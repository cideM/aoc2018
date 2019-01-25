#!/usr/bin/env stack
{-
    stack
    script
    --resolver lts-12.20
    --package vector,mtl
-}
{-# LANGUAGE RecordWildCards #-}

module Day8 where

import           Control.Arrow       ((&&&))
import           Control.Monad       as Monad
import           Control.Monad.State as State
import qualified Data.Maybe          as Maybe
import           Data.Semigroup      (Sum (..))
import           Data.Vector         (Vector, (!?))
import qualified Data.Vector         as Vector

data Node = Node
  { children :: Vector Node
  , metadata :: Vector Int
  } deriving (Show)

getNode :: State (Vector Int) Node
getNode = do
  x <- getOne
  y <- getOne
  Node <$> (Vector.fromList <$> Monad.replicateM x getNode) <*>
    (Vector.fromList <$> Monad.replicateM y getOne)

getOne :: State (Vector Int) Int
getOne = do
  xs <- State.get
  State.put $ Vector.tail xs
  return $ Vector.head xs

metaSum :: Node -> Int
metaSum Node {..} =
  Vector.sum metadata + Vector.foldl' (\a b -> a + metaSum b) 0 children

nodeValue :: Node -> Int
nodeValue Node {..} =
  if Vector.null children
    then Vector.sum metadata
    else sum . map nodeValue . Maybe.catMaybes . Vector.toList $
         Vector.map (\i -> children !? (i - 1)) metadata

main :: IO ()
main = do
  input <- Vector.fromList . map read . words <$> getContents
  print . (metaSum &&& nodeValue) $ State.evalState getNode input
