#!/usr/bin/env stack
{-
    stack
    script
    --resolver lts-13.16
    --package unordered-containers,hashable
-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Foldable
import           Data.Hashable
import           Data.HashSet               (HashSet)
import           Data.Text                  (Text)
import qualified Data.HashSet               as HashSet
import qualified Data.List                  as List
import           Data.Void                  (Void)
import           GHC.Generics               (Generic)
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Point =
  Point Int
        Int
        Int
        Int
  deriving (Show, Eq, Generic)

instance Hashable Point

isConnected :: HashSet Point -> Point -> Bool
isConnected set p = any (\p' -> distance p' p <= 3) set

distance :: Point -> Point -> Int
distance (Point a b c d) (Point a' b' c' d') =
  abs (a - a') + abs (b - b') + abs (c + c') + abs (d + d')

origin :: Point
origin = Point 0 0 0 0

constellation :: [Point] -> [HashSet Point]
constellation [] = []
constellation ps =
  let byDistFromOrigin = List.sortOn (distance origin) ps
   in go [] byDistFromOrigin
  where
    go out [] = out
    go out (p:ps) =
      let newConstellation =
            foldr
              (\p acc ->
                 if isConnected acc p
                   then HashSet.insert p acc
                   else acc)
              (HashSet.singleton p)
              ps
          remaining =
            HashSet.toList $
            HashSet.difference (HashSet.fromList ps) newConstellation
       in go (newConstellation : out) remaining

main :: IO ()
main = print "Hello"

-- ############# PARSING
type Parser = Parsec Void Text

inputParser :: Parser [Point]
inputParser = many pointParser <* space
  where
    pointParser = signedInteger <* space `sepBy` char ','
    lexeme = L.lexeme space
    integer = lexeme L.decimal
    signedInteger = L.signed space integer
