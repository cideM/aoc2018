{-# LANGUAGE OverloadedStrings #-}

module Day16 where

import           Control.Monad.State.Lazy (State)
import qualified Control.Monad.State.Lazy as State
import           Data.Bits                ((.&.), (.|.))
import           Data.IntMap.Strict       (IntMap, (!))
import qualified Data.IntMap.Strict       as IntMap
import           Data.Text                (Text)
import qualified Data.Text                as Text
import qualified Text.Parser.Token        as Token
import           Text.Trifecta            (Parser)
import qualified Text.Trifecta            as Tri
import           Types                    hiding (Input)

type Registers = IntMap Int

newtype Register =
  Register Int

newtype Immediate =
  Immediate Int

type OpCode = Int

data Instruction =
  Instruction OpCode
              Int
              Int
              Register

data Action
  = ADDR Register
         Register
         Register
  | ADDI Register
         Immediate
         Register
  | MULR Register
         Register
         Register
  | MULI Register
         Immediate
         Register
  | BANR Register
         Register
         Register
  | BANI Register
         Immediate
         Register
  | BORR Register
         Register
         Register
  | BORI Register
         Immediate
         Register
  | SETR Register
         Register
  | SETI Immediate
         Register
  | GTIR Immediate
         Register
         Register
  | GTRI Register
         Immediate
         Register
  | GTRR Register
         Register
         Register
  | EQRI Immediate
         Register
         Register
  | EQIR Register
         Immediate
         Register
  | EQRR Register
         Register
         Register

registersP :: Parser Registers
registersP = do
  _ <- Tri.string "Before:"
  _ <- Tri.space
  _ <- Tri.symbol "["
  contents <- Tri.decimal `Tri.sepBy` Tri.symbol ","
  _ <- Tri.symbol "]"
  return . IntMap.fromList . zip [0 ..] $ map fromIntegral contents

instructionP :: Parser Instruction
instructionP = do
  opCode <- fromIntegral <$> Tri.decimal
  Tri.spaces
  x <- fromIntegral <$> Tri.decimal
  Tri.spaces
  y <- fromIntegral <$> Tri.decimal
  Tri.spaces
  (Instruction opCode x y . Register) . fromIntegral <$> Tri.decimal

-- Split on double new line, discard 2nd part for now
-- Parse lines, convert to vector, chunksOf 3

runAction :: Registers -> Action -> Registers
runAction regs action =
  case action of
    ADDR (Register regA) (Register regB) (Register regC) ->
      let x = regs ! regA
          y = regs ! regB
       in insert regC (x + y)
    ADDI (Register regA) (Immediate y) (Register regC) ->
      let x = regs ! regA
       in insert regC (x + y)
    MULR (Register regA) (Register regB) (Register regC) ->
      let x = regs ! regA
          y = regs ! regB
       in insert regC (x * y)
    MULI (Register regA) (Immediate y) (Register regC) ->
      let x = regs ! regA
       in insert regC (x * y)
    BANR (Register regA) (Register regB) (Register regC) ->
      let x = regs ! regA
          y = regs ! regB
       in insert regC (x .&. y)
    BANI (Register regA) (Immediate y) (Register regC) ->
      let x = regs ! regA
       in insert regC (x .&. y)
    BORR (Register regA) (Register regB) (Register regC) ->
      let x = regs ! regA
          y = regs ! regB
       in insert regC (x .|. y)
    BORI (Register regA) (Immediate y) (Register regC) ->
      let x = regs ! regA
       in insert regC (x .|. y)
    SETR (Register regA) (Register regC) -> insert regC (regs ! regA)
    SETI (Immediate x) (Register regC) -> insert regC x
    GTIR (Immediate x) (Register regB) (Register regC) ->
      let y = regs ! regB
          v =
            if x > y
              then 1
              else 0
       in insert v regC
    GTRI (Register regA) (Immediate y) (Register regC) ->
      let x = regs ! regA
          v =
            if x > y
              then 1
              else 0
       in insert v regC
    GTRR (Register regA) (Register regB) (Register regC) ->
      let x = regs ! regA
          y = regs ! regB
          v =
            if x > y
              then 1
              else 0
       in insert v regC
    EQRI (Immediate x) (Register regB) (Register regC) ->
      let y = regs ! regB
          v =
            if x == y
              then 1
              else 0
       in insert v regC
    EQIR (Register regA) (Immediate y) (Register regC) ->
      let x = regs ! regA
          v =
            if x == y
              then 1
              else 0
       in insert v regC
    EQRR (Register regA) (Register regB) (Register regC) ->
      let x = regs ! regA
          y = regs ! regB
          v =
            if x == y
              then 1
              else 0
       in insert v regC
  where
    insert k v = IntMap.insert k v regs

run :: Text -> Either ErrMsg Text
run t = Right "foo"

prog :: DayProg
prog = DayProg "day16" run
