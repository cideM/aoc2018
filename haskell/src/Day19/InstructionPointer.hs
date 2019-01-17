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


data InstructionPointer = InstructionPointer
  { register :: Register
  , value    :: Int
  } deriving (Show, Eq)


run
  :: InstructionPointer
  -> Registers
  -> Vector Instruction
  -> Either ErrMsg (InstructionPointer, Registers)
run ip@InstructionPointer {..} registers instructions =
  let instruction = (Vector.!) instructions value -- ^ Get instruction at index (instruction pointer value)
  in  case opName instruction of
        Nothing -> -- ^ TODO: Maybe Instruction should not have opcode nor name, just be tuple of (name/code, instr)
          Left $ "No opName for instruction " <> Text.pack (show instruction)
        Just name ->
          let Register r  = register
              withIpValue = IntMap.insert r value registers -- ^ Insert ip value into register
          in  case Map.lookup name Op.ops of
                Nothing -> Left $ "Unkown op " <> name
                Just op ->
                  let regs'  = op instruction withIpValue
                      value' = (regs' ! r) + 1
                  in  if value' < 0 || value' > Vector.length instructions - 1
                        then Right (ip, regs')
                        else
                          let ip' = InstructionPointer (Register r) value'
                          in  run ip' regs' instructions
