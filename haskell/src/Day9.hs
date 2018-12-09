{-# LANGUAGE OverloadedStrings #-}

-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE DeriveFoldable #-}
-- {-# LANGUAGE DeriveTraversable #-}
module Day9 where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Ord as Ord
import qualified Data.Text as Text
import Data.Text (Text)
import Text.Parser.Char
import Text.Trifecta
import Types

type Marble = Int

type Player = Int

turns :: Int -> Int -> [(Player, Marble)]
turns playerCount maxMarble =
  take maxMarble . zip (concat $ repeat [1 .. playerCount]) $
  concat (repeat [1 .. maxMarble])

run :: Text -> Either ErrMsg Text
run t =
  let input = parseString inputP mempty $ Text.unpack t
   in case input of
        (Failure _) -> Left "Parsing failed"
        (Success (playerCount, maxMarble)) ->
          let (_, score1) = game $ turns playerCount maxMarble
              highscore1 = List.maximum $ Map.elems score1
              (_, score2) = game $ turns playerCount (maxMarble * 100)
              highscore2 = List.maximum $ Map.elems score2
           in Right . Text.pack $ show highscore1 ++ show highscore2

inputP :: Parser (Player, Marble)
inputP = do
  playerCount <- read <$> some digit
  _ <- skipSome (choice [space, char ';', letter])
  marbleCount <- read <$> some digit
  return (playerCount, marbleCount)

-- | Super nice, it only took me about ~4h to understand how a circle works. ~_~
-- Instead of trying to calculate the correct index at which to split the list,
-- we just rotate the circle after every insertion so the inserted part is
-- always our index 0.
-- xxxxxxxxxxxxxxx
--   ^ insert here
-- ll  rrrrrrrrrr
--    ^ split
-- ll o rrrrrrrrrr
--    ^ insert
-- o rrrrrrrrrr ll
-- ^^^^^^^^^^ rotate and join
-- The list is different of course but since it represents a circle it's still a
-- valid sequence
insertMarble :: a -> [a] -> [a]
insertMarble x [x'] = [x', x]
insertMarble x [x', x''] = [x', x, x'']
insertMarble x xs =
  let (left, right) = splitAt 2 xs
   in x : right ++ left

type CurrentIndex = Int

game :: [(Player, Marble)] -> ([Marble], Map Player Int)
game = Foldable.foldl' f ([0], Map.empty)
  where
    f :: ([Marble], Map Player Int)
      -> (Player, Marble)
      -> ([Marble], Map Player Int)
    f (circle, score) (player, marble)
      -- | Scoring turn, remove marble, adjust score
      | marble `rem` 23 == 0 =
        let (left, x:xs) = splitAt (length circle - 7) circle
            newScore = Map.insertWith (+) player (marble + x) score

         in (xs ++ left, newScore)
      -- | Normal turn, just add marble
      | otherwise =
        let newCircle = insertMarble marble circle
         in (newCircle, score)


prog :: DayProg
prog = DayProg "day9" run
