{-# LANGUAGE OverloadedStrings #-}

module Day19.InstructionPointerSpec where

import           Test.Hspec

import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as Vector
import           Day16.Types                    ( Instruction(..)
                                                , Register(..)
                                                )
import           Day19.InstructionPointer
import qualified Data.IntMap                   as IntMap

spec :: Spec
spec =
    describe "run"
        $ it "handles test case"
        $ let
              ip = InstructionPointer (Register 0) 0
              is = Vector.fromList
                  [ Instruction 0 (Just "seti") 5 0 (Register 1)
                  , Instruction 0 (Just "seti") 6 0 (Register 2)
                  , Instruction 0 (Just "addi") 0 1 (Register 0)
                  , Instruction 0 (Just "addr") 1 2 (Register 3)
                  , Instruction 0 (Just "setr") 1 0 (Register 0)
                  , Instruction 0 (Just "seti") 8 0 (Register 4)
                  , Instruction 0 (Just "seti") 9 0 (Register 5)
                  ]
              regs = IntMap.fromList
                  [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0)]
          in
              run ip regs is `shouldBe` Right
                  ( InstructionPointer (Register 0) 6
                  , IntMap.fromList
                      [(0, 6), (1, 5), (2, 6), (3, 0), (4, 0), (5, 9)]
                  )
