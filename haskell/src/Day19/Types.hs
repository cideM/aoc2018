module Day19.Types where

import           Day16.Types (Register)

data InstructionPointer = InstructionPointer
  { register :: Register
  , value    :: Int
  } deriving (Show, Eq)
