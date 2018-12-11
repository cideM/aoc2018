{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day10 where

import Control.Applicative ((<|>))
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import Data.Sequence (Seq((:<|)), Seq, (<|), (><), (|>))
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Debug.Trace as Trace
import Text.Parser.Char
import Text.Trifecta
import Types

data Star = Star
  { x :: !Int
  , y :: !Int
  , dx :: !Int
  , dy :: !Int
  , age :: !Int
  } deriving (Show, Eq, Ord)

-- | Why is this not part of the package? Or am I just too blind to find it?
intP :: Parser Int
intP = read <$> ((:) <$> option ' ' (char '-') <*> some digit)

inputP :: Parser Star
inputP =
  Star <$> (string "position=<" *> spaces *> intP <* char ',') <*>
  (spaces *> intP <* char '>' <* spaces) <*>
  (string "velocity=<" *> spaces *> intP <* char ',' <* spaces) <*>
  (spaces *> intP <* char '>' <* spaces) <*>
  pure 0

step :: Star -> Star
step s@Star {..} = s {x = x + dx, y = y + dy, age = age + 1}

makeGrid :: [Star] -> [[Text]]
makeGrid stars =
  Split.chunksOf
    (maxX + 1)
    [ maybe
      "."
      (const "o")
      -- | Get stars fo this row and then insert "o" if there's a star for that
      -- particular column
      (Map.lookup y m >>= List.find (\Star {x = x'} -> x' == x))
    | y <- [0 .. maxY]
    , x <- [0 .. maxX]
    ]
  where
    adjusted = fmap (\s@Star {..} -> s {x = x - minX, y = y - minY}) stars
      -- ^ Use min star as origin and adjust all others so we always start at 0,0
    (minX, minY) = minimum $ fmap (\s -> (x s, y s)) stars
    (maxX, maxY) = maximum $ fmap (\s -> (x s, y s)) stars
    m :: Map Int [Star]
    m =
      Foldable.foldr'
        (\star -> Map.insertWith (++) (y star) [star])
        Map.empty
        adjusted

-- | Not really size of a grid but rather how far apart the top left and bottom
-- right points are.
size :: [Star] -> Int
size stars = (maxX - minX) + (maxY - minY)
  where
    xy = fmap (\s -> (x s, y s)) stars
    (maxX, maxY) = maximum xy
    (minX, minY) = minimum xy

run :: Text -> Either ErrMsg Text
run t =
  case traverse (parse . Text.unpack) $ Text.lines t of
    Failure e -> Left $ "Parsing failed" <> Text.pack (show e)
    Success stars -> Right $ grid <> " " <> steps
      where stars' =
              List.minimumBy (\s1 s2 -> compare (size s1) (size s2)) .
              take 15000 $
              iterate (fmap step) stars
            steps = Text.pack . show . age $ head stars'
            grid = Text.unlines . fmap Text.unwords $ makeGrid stars'
  where
    parse = parseString inputP mempty

prog :: DayProg
prog = DayProg "day9" run
