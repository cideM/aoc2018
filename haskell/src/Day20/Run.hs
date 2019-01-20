{-# LANGUAGE OverloadedStrings #-}

module Day20.Run
  ( run
  ) where

import           Data.Text    (Text)
import           Data.Vector  (Vector)
import qualified Data.Vector  as Vector
import           Day20.Parser (parseInput)
import           Day20.Types
import           Types

flattenPaths :: [Path] -> [[Direction]]
flattenPaths paths = go paths [[]]
  where
    go [] out = out
    go (Step dir:rest) acc =
      let acc' = (++) <$> acc <*> [[dir]]
       in go rest acc'
    go (Branches as:rest) acc =
      let acc' = (++) <$> acc <*> concatMap flattenPaths as
       in go rest acc'

run :: Text -> Either ErrMsg Text
run input = do
  paths <- parseInput input
  return "Foo"
