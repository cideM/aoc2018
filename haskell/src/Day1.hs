{-# LANGUAGE OverloadedStrings #-}

module Day1
  ( prog
  )
where

import           Data.Text                     as Text
import           Types
import           Data.Text.Read                as Read
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set


type Sum = Int

type FirstRepeat = Maybe Int

data Day1Result = Day1Result !Sum !FirstRepeat deriving (Show)

transformInput :: Text -> Either ErrMsg [Int]
transformInput d = case res of
  (Left  e) -> Left $ Text.pack e
  -- ^ Not sure why Data.Text.Read has a Reader with String, rather than Text
  -- Here we're just transforming Either String a into Either Text a
  (Right i) -> Right i
  where res = traverse (fmap fst . Read.signed Read.decimal) $ Text.lines d

-- | firstDuplicate takes a list of integers. Starting with 0, it adds each
-- integer to the last result. It checks if that result is already known. If
-- so, return that result. It's the first duplicate result in our list of
-- integer additions. If the result is unknown, add it to the set of known
-- results. Iterate until we die, or find a result. #AllOrNothing.
firstDuplicate :: [Int] -> Maybe Int
firstDuplicate xs = go (Prelude.concat $ Prelude.repeat xs) 0 $ Set.singleton 0
 where
  go [] _ _ = Nothing
  go (x : xs') acc set =
    let res = acc + x
    in  if Set.member res set then Just res else go xs' res (Set.insert res set)

run :: Text -> Either ErrMsg Showable
run input = transformInput input
  >>= \xs -> Right . Types.pack $ Day1Result (sum xs) (firstDuplicate xs)

prog :: DayProg
prog = DayProg "day1" run
