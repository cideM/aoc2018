#!/usr/bin/env stack
{-
    stack
    script
    --resolver lts-12.20
    --package text,trifecta,containers,parsers,mtl,vector
-}
{-# LANGUAGE RecordWildCards #-}

import           Control.Applicative         ((<|>))
import           Control.Monad.Primitive     (PrimMonad, PrimState)
import qualified Control.Monad.ST            as ST
import           Data.Bits                   ((.&.), (.|.))
import qualified Data.IntSet                 as IntSet
import           Data.Vector                 (Vector)
import qualified Data.Vector                 as Vector
import           Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Unboxed.Mutable as UnboxedMut
import           Text.Trifecta

type Registers s = MVector s Int

type Register = Int

data Instruction = Instruction
  { kind :: !OpKind
  , a    :: !Int
  , b    :: !Int
  , out  :: !Register
  } deriving (Show, Eq)

type IP = (Register, Int)

data OpKind
  = Addr
  | Addi
  | Mulr
  | Muli
  | Banr
  | Bani
  | Borr
  | Bori
  | Setr
  | Seti
  | Gtir
  | Gtri
  | Gtrr
  | Eqri
  | Eqir
  | Eqrr
  deriving (Show, Eq)

runProgram ::
     (PrimMonad m)
  => IP
  -> Vector Instruction
  -> Registers (PrimState m)
  -> m Int
runProgram (register, initialValue) instructions registers =
  go IntSet.empty initialValue 0
  where
    go seen pointerValue lastSolution = do
      UnboxedMut.write registers register pointerValue -- ^ Write current pointer value to register
      let instruction = Vector.unsafeIndex instructions pointerValue -- ^ Retrieve current instruction pointed at by pointer value
      run instruction -- ^ Run that instruction on registers
      nextPointerValue <- (1 +) <$> UnboxedMut.read registers register -- ^ Retrieve register with pointer value, increment
      if pointerValue == 28
        then do
          value <- UnboxedMut.read registers 5
          if IntSet.member value seen
            then return lastSolution
            else go (IntSet.insert value seen) nextPointerValue value
        else go seen nextPointerValue lastSolution
    run Instruction {..} = do
      let len = UnboxedMut.length registers
      valueA <-
        if a < 0 || a >= len
          then return 0
          else UnboxedMut.read registers a
      valueB <-
        if b < 0 || b >= len
          then return 0
          else UnboxedMut.read registers b
      let result =
            case kind of
              Addr -> valueA + valueB
              Addi -> valueA + b
              Mulr -> valueA * valueB
              Muli -> valueA * b
              Banr -> valueA .&. valueB
              Bani -> valueA .&. b
              Borr -> valueA .|. valueB
              Bori -> valueA .|. b
              Setr -> valueA
              Seti -> a
              Gtir ->
                if a > valueB
                  then 1
                  else 0
              Gtri ->
                if valueA > b
                  then 1
                  else 0
              Gtrr ->
                if valueA > valueB
                  then 1
                  else 0
              Eqri ->
                if valueA == b
                  then 1
                  else 0
              Eqir ->
                if a == valueB
                  then 1
                  else 0
              Eqrr ->
                if valueA == valueB
                  then 1
                  else 0
      UnboxedMut.write registers out result

main :: IO ()
main = do
  input <- parseString inputP mempty <$> getContents
  case input of
    Failure parseError -> print parseError
    Success (ip, instructions) ->
      let result =
            ST.runST $ UnboxedMut.replicate 6 0 >>= runProgram ip instructions
       in print result

-- Parsing stuff
instructionPointerP :: Parser IP
instructionPointerP =
  (,) <$> (string "#ip" *> whiteSpace *> (fromIntegral <$> natural)) <*> pure 0

instructionP :: Parser Instruction
instructionP = Instruction <$> opKindP <*> number' <*> number' <*> number'
  where
    number' = fromIntegral <$> (whiteSpace *> natural)
    opKindP =
      Addr <$ string "addr" <|> Addi <$ string "addi" <|> Mulr <$ string "mulr" <|>
      Muli <$ string "muli" <|>
      Banr <$ string "banr" <|>
      Bani <$ string "bani" <|>
      Borr <$ string "borr" <|>
      Bori <$ string "bori" <|>
      Setr <$ string "setr" <|>
      Seti <$ string "seti" <|>
      Gtir <$ string "gtir" <|>
      Gtri <$ string "gtri" <|>
      Gtrr <$ string "gtrr" <|>
      Eqir <$ string "eqir" <|>
      Eqri <$ string "eqri" <|>
      Eqrr <$ string "eqrr"

inputP :: Parser (IP, Vector Instruction)
inputP =
  (,) <$> instructionPointerP <* whiteSpace <*>
  (Vector.fromList <$> many instructionP)
