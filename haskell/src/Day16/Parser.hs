{-# LANGUAGE OverloadedStrings #-}

module Day16.Parser
  ( parseInput
  ) where

import           Control.Applicative   ((<|>))
import qualified Data.IntMap.Strict    as IntMap
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Day16.Types
import           Text.Parser.LookAhead (lookAhead)
import           Text.Trifecta         (Parser, Result (Failure, Success))
import qualified Text.Trifecta         as Tri
import           Types

registersP :: Parser Registers
registersP = do
  _ <- Tri.string "Before:" <|> Tri.string "After:"
  _ <- Tri.spaces
  _ <- Tri.char '['
  contents <- Tri.decimal `Tri.sepBy` Tri.symbol ","
  _ <- Tri.char ']'
  return . IntMap.fromList . zip [0 ..] $ map fromIntegral contents

instructionP :: Parser Instruction
instructionP = do
  opCode <- dec
  x <- dec
  y <- dec
  Instruction opCode x y . Register <$> dec
  where
    dec = fromIntegral <$> (Tri.spaces *> Tri.decimal)

inputTripletP :: Parser Sample
inputTripletP = do
  before <- registersP <* Tri.spaces
  instruction <- instructionP <* Tri.spaces
  after <- registersP <* Tri.spaces
  return (before, instruction, after)

inputP :: Parser ([Sample], [Instruction])
inputP = do
  p1input <-
    Tri.manyTill inputTripletP (Tri.try . lookAhead $ Tri.count 2 instructionP)
  p2input <- Tri.many $ instructionP <* Tri.spaces
  return (p1input, p2input)

parseInput :: Text -> Either ErrMsg ([Sample], [Instruction])
parseInput input =
  case Tri.parseString inputP mempty str of
    Failure err -> Left . Text.pack $ show err
    Success res -> Right res
  where
    str = Text.unpack input
