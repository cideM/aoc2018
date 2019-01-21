{-# LANGUAGE OverloadedStrings #-}

module Day9 (run) where

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq((:<|)), Seq, (<|), (><), (|>))
import qualified Data.Text as Text
import Data.Text (Text)
import Text.Parser.Char
import Text.Trifecta
import Types

type Marble = Int

type Player = Int

type PlayerCount = Int

type MarbleCount = Int

turns :: PlayerCount -> MarbleCount -> [(Player, Marble)]
turns pCount mCount =
  take mCount . zip (concat $ repeat [1 .. pCount]) $
  concat (repeat [1 .. mCount])

run :: Text -> Either ErrMsg Text
run t =
  let input = parseString inputP mempty $ Text.unpack t
   in case input of
        (Failure _) -> Left "Parsing failed"
        (Success (pCount, mCount)) ->
          let (_, score) = game $ turns pCount mCount
              (_, score2) = game $ turns pCount (mCount * 100)
              getHighscore = List.maximum . Map.elems
           in Right . Text.pack $
              show (getHighscore score) ++ " " ++ show (getHighscore score2)

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
insertMarble :: a -> Seq a -> Seq a
insertMarble x xs
  | Seq.length xs == 1 = xs |> x
  | Seq.length xs == 2 =
    let (l, r) = Seq.splitAt 1 xs
     in (l |> x) >< r
  | otherwise =
    let (l, r) = Seq.splitAt 2 xs
     in (x <| r) >< l

game :: [(Player, Marble)] -> (Seq Marble, Map Player Int)
game = Foldable.foldl' f (Seq.singleton 0, Map.empty)
  where
    f :: (Seq Marble, Map Player Int)
      -> (Player, Marble)
      -> (Seq Marble, Map Player Int)
    f (circle, score) (player, marble)
      -- | Scoring turn, remove marble, adjust score
      | marble `rem` 23 == 0 =
        let (left, x :<| xs) = Seq.splitAt (length circle - 7) circle
            newScore = Map.insertWith (+) player (marble + x) score
         in (xs >< left, newScore)
      -- | Normal turn, just add marble
      | otherwise =
        let newCircle = insertMarble marble circle
         in (newCircle, score)
