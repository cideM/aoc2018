{-# LANGUAGE OverloadedStrings #-}

module Day20.Parser where

import           Control.Applicative ((<|>))
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Day20.Types
import           Text.Trifecta       (Parser, Result (Failure, Success))
import qualified Text.Trifecta       as Tri
import           Types

openParen :: Parser String
openParen = Tri.symbol "("

closeParen :: Parser String
closeParen = Tri.symbol ")"

baseP :: Parser [[Path]]
baseP = Tri.between (Tri.symbol "^") (Tri.symbol "$") segmentP

segmentP :: Parser [[Path]]
segmentP = Tri.many pathP `Tri.sepBy` Tri.symbol "|"

pathP :: Parser Path
pathP = (Step <$> directionP) <|> (Branches <$> branchesP)
  where
    branchesP = Tri.between openParen closeParen segmentP

directionP :: Parser Direction
directionP =
  (N <$ Tri.char 'N') <|> (S <$ Tri.char 'S') <|> (W <$ Tri.char 'W') <|>
  (E <$ Tri.char 'E')

parseInput :: Text -> Either ErrMsg [[Path]]
parseInput input =
  case Tri.parseString baseP mempty $ Text.unpack input of
    Failure err -> Left . Text.pack $ show err
    Success res -> Right res
