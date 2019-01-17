{-# LANGUAGE OverloadedStrings #-}

module Day19.Parser
  ( parseInput
  , instructionP
  )
where

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as Vector
import           Day16.Types                    ( Instruction(..)
                                                , Register(..)
                                                )
import           Day19.InstructionPointer
import           Text.Trifecta                  ( Parser
                                                , Result(Failure, Success)
                                                )
import qualified Text.Trifecta                 as Tri
import           Types

instructionPointerP :: Parser InstructionPointer
instructionPointerP =
  InstructionPointer
    <$> (  Tri.string "#ip"
        *> Tri.whiteSpace
        *> (Register . fromIntegral <$> Tri.natural)
        )
    <*> pure 0

instructionP :: Parser Instruction
instructionP =
  Instruction 0 <$> opName' <*> number' <*> number' <*> (Register <$> number')
  -- | The opCode is not really 0 but I don't feel like refactoring everything
  -- from OpCode to Maybe OpCode.
 where
  number' = fromIntegral <$> Tri.natural <* Tri.whiteSpace
  opName' =
    Just
      .   Text.concat
      .   map Text.singleton
      <$> Tri.many Tri.letter
      <*  Tri.whiteSpace

inputP :: Parser (InstructionPointer, Vector Instruction)
inputP =
  (,)
    <$> instructionPointerP
    <*  Tri.whiteSpace
    <*> (Vector.fromList <$> Tri.many instructionP)

parseInput :: Text -> Either ErrMsg (InstructionPointer, Vector Instruction)
parseInput input = case Tri.parseString inputP mempty $ Text.unpack input of
  Failure err -> Left . Text.pack $ show err
  Success res -> Right res
