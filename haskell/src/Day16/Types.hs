module Day16.Types where

import           Data.IntMap.Strict (IntMap)

type Registers = IntMap Int

newtype Register =
  Register Int
  deriving (Show, Eq, Ord)

type OpCode = Int

data Instruction = Instruction
  { opCode :: OpCode
  , a      :: Int
  , b      :: Int
  , out    :: Register
  } deriving (Show, Eq, Ord)

type Sample = (Registers, Instruction, Registers)
