#!/usr/bin/env stack
{-
    stack
    script
    --resolver lts-12.20
    --package text,trifecta,containers,parsers,mtl,vector
    --
    -O2
-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- This is ugly as fuck but I hate those elf code exercises especially the ones
-- where you need to manually analyse the input. Part 2 is incredibly slow.
import           Control.Applicative         ((<|>))
import           Data.Bits                   ((.&.), (.|.))
import           Data.IntSet                 (IntSet)
import qualified Data.IntSet                 as IntSet
import           Data.String                 (IsString)
import           Data.Vector                 (Vector)
import qualified Data.Vector                 as Vector
import qualified Data.Vector.Generic.Mutable as M
import           Data.Vector.Unboxed         ((!))
import qualified Data.Vector.Unboxed         as Unboxed
import           Debug.Trace
import           Text.Trifecta

type Registers = Unboxed.Vector Int

type Register = Int

type OpName = String

data Instruction = Instruction
  { kind :: OpKind
  , a    :: Int
  , b    :: Int
  , out  :: Register
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

run :: Instruction -> Registers -> Registers
run Instruction {..} rs =
  let result =
        case kind of
          Addr -> rs ! a + rs ! b
          Addi -> rs ! a + b
          Mulr -> rs ! a * rs ! b
          Muli -> rs ! a * b
          Banr -> rs ! a .&. rs ! b
          Bani -> rs ! a .&. b
          Borr -> rs ! a .|. rs ! b
          Bori -> rs ! a .|. b
          Setr -> rs ! a
          Seti -> a
          Gtir ->
            if a > rs ! b
              then 1
              else 0
          Gtri ->
            if rs ! a > b
              then 1
              else 0
          Gtrr ->
            if rs ! a > rs ! b
              then 1
              else 0
          Eqri ->
            if rs ! a == b
              then 1
              else 0
          Eqir ->
            if a == rs ! b
              then 1
              else 0
          Eqrr ->
            if rs ! a == rs ! b
              then 1
              else 0
   in rs Unboxed.// [(out, result)]

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

runProgram :: IP -> Vector (Registers -> Registers) -> Registers -> Maybe Int
runProgram (register, initialValue) program initialRegisters =
  go IntSet.empty initialRegisters initialValue 0
  where
    go seen registers !pointerValue last =
      case program Vector.!? pointerValue of
        Nothing -> Nothing
        Just fn ->
          let nextRegisters =
                fn $
                Unboxed.modify
                  (\vec -> M.write vec register pointerValue)
                  registers
              -- ^ Run instruction on registers after inserting pointer value
              nextPointerValue = (nextRegisters Unboxed.! register) + 1
              -- ^ Retrieve register with pointer value, increment
           in if pointerValue == 28
                then let value = registers Unboxed.! 5
                      in if IntSet.member value seen
                           then Just last
                           else let seen' = IntSet.insert value seen
                                 in go
                                      (trace (show $ IntSet.size seen') seen')
                                      nextRegisters
                                      nextPointerValue
                                      value
                else go seen nextRegisters nextPointerValue last

main :: IO ()
main = do
  input <- parseString inputP mempty <$> getContents
  case input of
    Failure err -> print err
    Success (ip, instructions) ->
      let program = Vector.map run instructions
          !result = runProgram ip program (Unboxed.fromList [0, 0, 0, 0, 0, 0])
       in print result
