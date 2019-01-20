{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Day19.InstructionPointer where

import qualified Day16.Operation          as Op
import           Day16.Types              (Register (..), Registers)

import           Control.Monad.State.Lazy (State)
import qualified Control.Monad.State.Lazy as State
import           Data.IntMap.Strict       ((!))
import qualified Data.IntMap.Strict       as IntMap
import qualified Data.Map.Strict          as Map
import           Data.String              (IsString)
import           Data.Vector              (Vector)
import qualified Data.Vector              as Vector
import           Day19.Types              (InstructionPointer (..),
                                           InstructionWithName)
import           Types

-- | Maybe to Either.
mb2E :: (IsString a) => a -> Maybe b -> Either a b
mb2E = flip maybe Right . Left

-- | Attempt at using State monad. While this can be written with iterateWhile
-- so it exits correctly, it will break when an op isn't found because it will
-- just keep iterating. Sigh.
makeState ::
     Vector InstructionWithName
  -> State (InstructionPointer, Registers) (Either ErrMsg Registers)
makeState instructions = do
  (InstructionPointer {..}, registers) <- State.get
  let Register r = register
      withIpValue = IntMap.insert r value registers -- ^ Insert ip value into register
      (opName, instruction) = (Vector.!) instructions value -- ^ Get instruction at index (instruction pointer value)
  case Map.lookup opName Op.ops of
    Nothing -> return . Left $ "Unkown op " <> opName
    Just operation -> do
      let newRegisters = operation instruction withIpValue
          newValue = (newRegisters ! r) + 1
      State.put (InstructionPointer (Register r) newValue, newRegisters)
      return $ Right newRegisters

run ::
     InstructionPointer
  -> Registers
  -> Vector InstructionWithName
  -> Either ErrMsg (InstructionPointer, Registers)
run ip@InstructionPointer {..} registers instructions = do
  let (opName, instruction) = (Vector.!) instructions value -- ^ Get instruction at index (instruction pointer value)
      Register r = register
      withIpValue = IntMap.insert r value registers -- ^ Insert ip value into register
  operation <- mb2E ("Unkown op " <> opName) (Map.lookup opName Op.ops)
  let newRegisters = operation instruction withIpValue
      newValue = (newRegisters ! r) + 1
  if newValue < 0 || newValue > Vector.length instructions - 1
    then Right (ip, newRegisters)
    else run
           (InstructionPointer (Register r) newValue)
           newRegisters
           instructions
