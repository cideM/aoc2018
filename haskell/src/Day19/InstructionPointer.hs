{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Day19.InstructionPointer where

import           Day16.Types                    ( Register(..)
                                                , Registers
                                                , Instruction(..)
                                                )
import qualified Day16.Operation               as Op
import qualified Control.Monad                 as Monad
import           Types
import qualified Data.Map.Strict               as Map
import           Data.IntMap.Strict             ( (!) )
import qualified Data.IntMap.Strict            as IntMap
import qualified Data.Text                     as Text
import           Data.Vector                    ( Vector )
import           Debug.Trace
import qualified Data.Vector                   as Vector
import           Data.String                    ( IsString )


data InstructionPointer = InstructionPointer
  { register :: Register
  , value    :: Int
  } deriving (Show, Eq)

-- | Maybe to Either. It's SO FUCKING ANNOYING
mb2E :: (IsString a) => a -> Maybe b -> Either a b
mb2E = flip maybe Right . Left

run
  :: InstructionPointer
  -> Registers
  -> Vector Instruction
  -> Either ErrMsg (InstructionPointer, Registers)
run ip@InstructionPointer {..} registers instructions = do
  let instruction = (Vector.!) instructions value -- ^ Get instruction at index (instruction pointer value)

  name <- mb2E
    ("No opName for instruction " <> Text.pack (show instruction))
    (opName (trace (show instruction <> " " <> show ip) instruction))

  let Register r  = register
      withIpValue = IntMap.insert r value registers -- ^ Insert ip value into register

  operation <- mb2E ("Unkown op " <> name) (Map.lookup name Op.ops)

  let regs'  = operation instruction withIpValue
      value' = (regs' ! r) + 1

  if value' < 0 || value' > Vector.length instructions - 1
    then Right (ip, regs')
    else run (InstructionPointer (Register r) value')
             (traceShowId regs')
             instructions
