{-# LANGUAGE OverloadedStrings #-}

module Day19.ParserSpec where

import           Test.Hspec

import qualified Data.Vector   as Vector
import           Day16.Types   (Instruction (..), Register (..))
import           Day19.Parser  (instructionP, parseInput)
import           Day19.Types   (InstructionPointer (..))
import           Text.Trifecta (Parser, Result (Failure, Success))
import qualified Text.Trifecta as Tri

prs :: Parser a -> String -> Either String a
prs p s =
  let r = Tri.parseString p mempty s
   in case r of
        (Success v)   -> Right v
        (Failure err) -> Left $ show err

spec :: Spec
spec = do
  describe "parseInput" $
    it "parses input" $
    parseInput "#ip 4\naddi 4 16 4\nseti 1 5 3" `shouldBe`
    Right
      ( InstructionPointer (Register 4) 0
      , Vector.fromList
          [ ("addi", Instruction 4 16 (Register 4))
          , ("seti", Instruction 1 5 (Register 3))
          ])
  describe "instructionP" $
    it "parses instruction" $
    prs instructionP "seti 1 5 3" `shouldBe`
    Right ("seti", Instruction 1 5 (Register 3))
