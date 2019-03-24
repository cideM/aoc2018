#!/usr/bin/env stack
{-
    stack
    script
    --resolver lts-13.6
    --package transformers,mtl,megaparsec,text,vector
-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Data.Void                  (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

data Bot a = Bot
  { x :: a
  , y :: a
  , z :: a
  , r :: a
  } deriving (Show, Eq)

data Point a =
  Point a
        a
        a
  deriving (Show, Eq)

botParser :: Parser (Bot Integer)
botParser = do
  _ <- string "pos=<"
  _x <- signedDec <* char ','
  _y <- signedDec <* char ','
  _z <- signedDec
  _ <- string ">, r="
  _r <- signedDec
  return $ Bot _x _y _z _r
  where
    signedDec = L.signed space L.decimal

inputParser :: Parser (V.Vector (Bot Integer))
inputParser = V.fromList <$> many (botParser <* space)

distance :: Num a => Point a -> Point a -> a
distance (Point x1 y1 z1) (Point x2 y2 z2) =
  let x' = abs $ x2 - x1
      y' = abs $ y2 - y1
      z' = abs $ z2 - z1
   in x' + y' + z'

main :: IO ()
main = do
  parsed <- runParser inputParser "stdin" . T.pack <$> getContents
  case parsed of
    Left e -> print e
    Right bots -> do
      let strongest = V.maximumBy (\b1 b2 -> r b1 `compare` r b2) bots
          inRangeOfStrongest =
            V.filter (flip (<=) (r strongest) . distBots strongest) bots
      print $ "Strongest: " ++ show strongest
      print $ "In range of strongest: " ++ show (V.length inRangeOfStrongest)
  where
    distBots b1 b2 =
      distance (Point (x b1) (y b1) (z b1)) (Point (x b2) (y b2) (z b2))
{-
    Thoughts P2:
    Partition space into subspaces (cubes). Cube Origin Radius
    Pick cube with most nanobots in range. In case of tie, choose closest to origin (?)*.
    Rinse and repeat...?

    * I think this could be wrong. A bot from cube A could reach into cube B
    resulting in both having the same bots in range but cube B gets closer to
    origin.
-}
