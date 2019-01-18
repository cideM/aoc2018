module Day16.Types where

import           Data.IntMap.Strict (IntMap)
import           Data.Text          (Text)

type Registers = IntMap Int

newtype Register =
  Register Int
  deriving (Show, Eq, Ord)

type OpCode = Int

type OpName = Text

data Instruction = Instruction
  { a   :: Int
  , b   :: Int
  , out :: Register
  } deriving (Show, Eq, Ord)

type Sample = (Registers, InstructionWithCode, Registers)

type InstructionWithCode = (OpCode, Instruction)
