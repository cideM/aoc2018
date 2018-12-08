{-# LANGUAGE OverloadedStrings #-}

module Day8 where

import qualified Control.Monad as Monad
import Control.Monad.State.Lazy
import qualified Control.Monad.State.Lazy as State
import qualified Data.Text as Text
import Data.Text (Text)
import Types

type ChildCount = Int

type MetaCount = Int

data Node a =
  Node [Node a]
       [a] -- Children and meta data
  deriving (Show)

nodeFromInts :: State [Int] (Node Int)
nodeFromInts = do
  childCount <- getNext
  metaCount <- getNext
  Node <$> Monad.replicateM childCount nodeFromInts <*>
    Monad.replicateM metaCount getNext

getNext :: State [a] a
getNext = do
  (x:rest) <- State.get
  State.put rest
  pure x

run :: Text -> Either ErrMsg Text
run t =
  let input = map (read . Text.unpack) $ Text.words t
      tree = evalState nodeFromInts input
   in Right . Text.pack $ show tree

prog :: DayProg
prog = DayProg "day8" run
