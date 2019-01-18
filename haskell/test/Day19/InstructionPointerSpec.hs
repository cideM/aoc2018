{-# LANGUAGE OverloadedStrings #-}

module Day19.InstructionPointerSpec where

import           Test.Hspec

import qualified Data.IntMap              as IntMap
import qualified Data.Vector              as Vector
import           Day16.Types              (Instruction (..), Register (..))
import qualified Day19.InstructionPointer as IP
import           Day19.Types              (InstructionPointer (..))

spec :: Spec
spec =
  describe "run" $
  it "handles test case" $
  let ip = InstructionPointer (Register 0) 0
      is =
        Vector.fromList
          [ ("seti", Instruction 5 0 (Register 1))
          , ("seti", Instruction 6 0 (Register 2))
          , ("addi", Instruction 0 1 (Register 0))
          , ("addr", Instruction 1 2 (Register 3))
          , ("setr", Instruction 1 0 (Register 0))
          , ("seti", Instruction 8 0 (Register 4))
          , ("seti", Instruction 9 0 (Register 5))
          ]
      regs = IntMap.fromList [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0)]
   in IP.run ip regs is `shouldBe`
      Right
        ( InstructionPointer (Register 0) 6
        , IntMap.fromList [(0, 6), (1, 5), (2, 6), (3, 0), (4, 0), (5, 9)])
