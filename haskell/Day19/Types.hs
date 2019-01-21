module Day19.Types where

import           Data.Vector (Vector)
import           Day16.Types (Instruction, OpName, Register)

type InstructionWithName = (OpName, Instruction)

data InstructionPointer = InstructionPointer
  { register :: Register
  , value    :: Int
  } deriving (Show, Eq)

type Input = (InstructionPointer, Vector InstructionWithName)
