#!/usr/bin/env stack
{-
    stack
    script
    --resolver lts-12.20
    --package vector,trifecta,text
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Day10 where

import qualified Data.List     as List
import           Data.Text     (Text)
import qualified Data.Text     as Text
import qualified Data.Text.IO  as TIO
import           Data.Vector   (Vector)
import qualified Data.Vector   as Vector
import           Text.Trifecta

-- TODO: Refactor this file
data Star a = Star
  { x   :: !a
  , y   :: !a
  , dx  :: !a
  , dy  :: !a
  , age :: !a
  } deriving (Show, Eq, Ord)

instance Foldable Star where
  foldMap f s = f (x s) <> f (y s)

inputP :: Parser (Star Int)
inputP =
  Star <$> (string "position=<" *> intP <* char ',') <*>
  (intP <* char '>' <* spaces) <*>
  (string "velocity=<" *> intP <* char ',' <* spaces) <*>
  (intP <* char '>' <* spaces) <*>
  pure 0
  where
    intP = whiteSpace *> (fromIntegral <$> integer)

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

main :: IO ()
main = do
  input <- parseString (many $ whiteSpace *> inputP) mempty <$> getContents
  case input of
    Failure parseErr -> print parseErr
    Success stars -> do
      let stars' =
            List.minimumBy (\s1 s2 -> compare (size s1) (size s2)) $
            Vector.iterateN 15000 (fmap step) (Vector.fromList stars)
      print . age $ Vector.head stars'
      TIO.putStrLn . Text.unlines . fmap Text.unwords . Vector.toList $
        Vector.map Vector.toList (makeGrid stars')
