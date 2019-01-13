{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Day16.Operation
  ( Operation
  , OpName
  , name
  , run
  , fromInstruction
  , allNames
  ) where

import           Data.Bits          ((.&.), (.|.))
import           Data.IntMap.Strict ((!))
import qualified Data.IntMap.Strict as IntMap
import           Data.Text          (Text)
import           Day16.Types

type OpName = Text

data Operation = Operation
  { output :: Register
  , kind   :: OpKind
  , name   :: OpName
  } deriving (Ord, Eq, Show)

data OpKind
  = ADDR Register
         Register
  | ADDI Register
         Immediate
  | MULR Register
         Register
  | MULI Register
         Immediate
  | BANR Register
         Register
  | BANI Register
         Immediate
  | BORR Register
         Register
  | BORI Register
         Immediate
  | SETR Register
  | SETI Immediate
  | GTIR Immediate
         Register
  | GTRI Register
         Immediate
  | GTRR Register
         Register
  | EQRI Register
         Immediate
  | EQIR Immediate
         Register
  | EQRR Register
         Register
  deriving (Ord, Eq, Show)

run :: Registers -> Operation -> Registers
run regs Operation {..} =
  let Register outputReg = output
      result =
        case kind of
          ADDR (Register regA) (Register regB) ->
            let x = regs ! regA
                y = regs ! regB
             in x + y
          ADDI (Register regA) (Immediate y) ->
            let x = regs ! regA
             in x + y
          MULR (Register regA) (Register regB) ->
            let x = regs ! regA
                y = regs ! regB
             in x * y
          MULI (Register regA) (Immediate y) ->
            let x = regs ! regA
             in x * y
          BANR (Register regA) (Register regB) ->
            let x = regs ! regA
                y = regs ! regB
             in x .&. y
          BANI (Register regA) (Immediate y) ->
            let x = regs ! regA
             in x .&. y
          BORR (Register regA) (Register regB) ->
            let x = regs ! regA
                y = regs ! regB
             in x .|. y
          BORI (Register regA) (Immediate y) ->
            let x = regs ! regA
             in x .|. y
          SETR (Register regA) -> regs ! regA
          SETI (Immediate x) -> x
          GTIR (Immediate x) (Register regB) ->
            let y = regs ! regB
             in if x > y
                  then 1
                  else 0
          GTRI (Register regA) (Immediate y) ->
            let x = regs ! regA
             in if x > y
                  then 1
                  else 0
          GTRR (Register regA) (Register regB) ->
            let x = regs ! regA
                y = regs ! regB
             in if x > y
                  then 1
                  else 0
          EQRI (Register x) (Immediate regB) ->
            let y = regs ! regB
             in if x == y
                  then 1
                  else 0
          EQIR (Immediate regA) (Register y) ->
            let x = regs ! regA
             in if x == y
                  then 1
                  else 0
          EQRR (Register regA) (Register regB) ->
            let x = regs ! regA
                y = regs ! regB
             in if x == y
                  then 1
                  else 0
   in insert outputReg result
  where
    insert k v = IntMap.insert k v regs

fromInstruction :: Instruction -> OpName -> Maybe Operation
fromInstruction (Instruction _ inputA inputB output) opName = do
  opKind <-
    case opName of
      "addr" -> Just $ ADDR (Register inputA) (Register inputB)
      "addi" -> Just $ ADDI (Register inputA) (Immediate inputB)
      "mulr" -> Just $ MULR (Register inputA) (Register inputB)
      "muli" -> Just $ MULI (Register inputA) (Immediate inputB)
      "banr" -> Just $ BANR (Register inputA) (Register inputB)
      "bani" -> Just $ BANI (Register inputA) (Immediate inputB)
      "borr" -> Just $ BORR (Register inputA) (Register inputB)
      "bori" -> Just $ BORI (Register inputA) (Immediate inputB)
      "gtir" -> Just $ GTIR (Immediate inputA) (Register inputB)
      "gtri" -> Just $ GTRI (Register inputA) (Immediate inputB)
      "gtrr" -> Just $ GTRR (Register inputA) (Register inputB)
      "eqri" -> Just $ EQRI (Register inputA) (Immediate inputB)
      "eqir" -> Just $ EQIR (Immediate inputA) (Register inputB)
      "eqrr" -> Just $ EQRR (Register inputA) (Register inputB)
      "setr" -> Just $ SETR (Register inputA)
      "seti" -> Just $ SETI (Immediate inputA)
      _      -> Nothing
  return $ Operation output opKind opName

allNames :: [Text]
allNames =
  [ "addr"
  , "addi"
  , "mulr"
  , "muli"
  , "banr"
  , "bani"
  , "borr"
  , "bori"
  , "setr"
  , "seti"
  , "gtir"
  , "gtri"
  , "gtrr"
  , "eqri"
  , "eqir"
  , "eqrr"
  ]
