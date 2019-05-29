#!/usr/bin/env stack
{-
    stack
    script
    --resolver lts-13.16
    --package pretty-simple,megaparsec,text,fgl
-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative               (empty)
import           Data.Foldable
import qualified Data.Graph.Inductive.Graph        as Graph
import           Data.Graph.Inductive.PatriciaTree (UGr)
import           Data.Graph.Inductive.Query        as Query
import qualified Data.List                         as List
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Data.Void                         (Void)
import           Debug.Pretty.Simple
import           Text.Megaparsec                   hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer        as L
import           Text.Pretty.Simple                (pPrint)

data Point =
  Point Int
        Int
        Int
        Int
  deriving (Show, Eq)

distance :: Point -> Point -> Int
distance (Point a b c d) (Point a' b' c' d') =
  abs (a - a') + abs (b - b') + abs (c - c') + abs (d - d')

origin :: Point
origin = Point 0 0 0 0

main :: IO ()
main = do
  parsed <- runParser inputParser "stdin" . T.pack <$> getContents
  case parsed of
    Left e -> pPrint e
    Right points ->
      let withIds = zip [1 ..] points
          edges =
            [ (x, y)
            | (x, p1) <- withIds
            , (y, p2) <- withIds
            , distance p1 p2 <= 3
            ]
       in pPrint . Query.noComponents $
          (Graph.mkUGraph [1 .. length points] edges :: UGr)

-- ############# PARSING
type Parser = Parsec Void Text

inputParser :: Parser [Point]
inputParser =
  many $
  (Point <$> signedInteger <* comma <*> signedInteger <* comma <*> signedInteger <*
   comma <*>
   signedInteger) <*
  space
  where
    comma = L.symbol space ","
    signedInteger = L.signed space L.decimal
