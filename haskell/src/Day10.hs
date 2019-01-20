{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day10 (run) where

import qualified Data.List as List
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Text.Parser.Char
import Text.Trifecta
import Types

data Star a = Star
  { x :: !a
  , y :: !a
  , dx :: !a
  , dy :: !a
  , age :: !a
  } deriving (Show, Eq, Ord)

instance Foldable Star where
  foldMap f s = f (x s) <> f (y s)

-- | Why is this not part of the package? Or am I just too blind to find it?
intP :: Parser Int
intP = read <$> ((:) <$> option ' ' (char '-') <*> some digit)

inputP :: Parser (Star Int)
inputP =
  Star <$> (string "position=<" *> spaces *> intP <* char ',') <*>
  (spaces *> intP <* char '>' <* spaces) <*>
  (string "velocity=<" *> spaces *> intP <* char ',' <* spaces) <*>
  (spaces *> intP <* char '>' <* spaces) <*>
  pure 0

step :: Num a => Star a -> Star a
step s@Star {..} = s {x = x + dx, y = y + dy, age = age + 1}

makeGrid :: Vector (Star Int) -> Vector (Vector Text)
makeGrid stars =
  Vector.generate
    maxY
    (\y' ->
       let row = Vector.replicate maxX "."
           starsForRow =
             Vector.map (\s -> (x s, "o")) $
             Vector.filter (\s -> y s == y') stars'
       in Vector.update row starsForRow)
  where
    stars' = Vector.map (\s@Star {..} -> s {x = x - minX, y = y - minY}) stars
      -- ^ Use min star as origin and adjust all others so we always start at 0,0
    Star {x = maxX, y = maxY} = maximum stars
    Star {x = minX, y = minY} = minimum stars

-- | Not really size of a grid but rather how far apart the top left and bottom
-- right points are.
size :: Foldable t => t (Star Int) -> Int
size stars = (maxX - minX) + (maxY - minY)
  where
    Star {x = maxX, y = maxY} = maximum stars
    Star {x = minX, y = minY} = minimum stars

run :: Text -> Either ErrMsg Text
run t =
  case traverse (parse . Text.unpack) . Vector.fromList $ Text.lines t of
    Failure e -> Left $ "Parsing failed" <> Text.pack (show e)
    Success stars -> Right $ grid <> " " <> steps
      where stars' =
              List.minimumBy (\s1 s2 -> compare (size s1) (size s2)) $
              Vector.iterateN 15000 (fmap step) stars
            steps = Text.pack . show . age $ Vector.head stars'
            grid =
              Text.unlines . fmap Text.unwords . Vector.toList $
              Vector.map Vector.toList (makeGrid stars')
  where
    parse = parseString inputP mempty
