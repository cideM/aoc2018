{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

-- This is *not* my solution. I did this in Rust first and then looked at other
-- implementations. I was somewhat in awe when I saw the elegance of
-- https://github.com/glguy/advent2018/blob/master/execs/Day08.hs
-- So I went ahead and implemented it for myself, but referring to his code for
-- directions.
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
  Node [Node a] -- children
       [a] -- meta data
  deriving (Show, Functor, Foldable, Traversable)

nodeValue :: Node Int -> Int
nodeValue (Node xs ys)
  | null xs = sum ys
  | otherwise = sum $ fmap (maybe 0 nodeValue . getChildAtIndex) ys
  where
    getChildAtIndex i = indexOneBased i xs

indexOneBased :: Int -> [a] -> Maybe a
indexOneBased i xs
  | i' >= 0 && i' < length xs = Just $ xs !! i'
  | otherwise = Nothing
  where
    i' = i - 1

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
   in Right . Text.pack $ show (sum tree) ++ " " ++ show (nodeValue tree)

prog :: DayProg
prog = DayProg "day8" run
