{-# LANGUAGE OverloadedStrings #-}

module Day19.ParserSpec where

import           Test.Hspec

import           Data.Vector   (Vector)
import qualified Data.Vector   as Vector
import           Day16.Types   (Instruction (..), Register (..))
import           Day19.Parser  (instructionP, parseInput)
import           Day19.Types
import           Text.Trifecta (Parser, Result (Failure, Success))
import qualified Text.Trifecta as Tri

prs :: Parser a -> String -> Either String a
prs p s =
  let r = Tri.parseString p mempty s
   in case r of
        (Success a)   -> Right a
        (Failure err) -> Left $ show err

spec :: Spec
spec = do
  describe "parseInput" $
    it "parses input" $
    parseInput "#ip 4\naddi 4 16 4\nseti 1 5 3" `shouldBe`
    Right
      ( InstructionPointer (Register 4) 0
      , Vector.fromList
          [ Instruction 0 (Just "addi") 4 16 (Register 4)
          , Instruction 0 (Just "seti") 1 5 (Register 3)
          ])
  describe "instructionP" $
    it "parses instruction" $
    prs instructionP "seti 1 5 3" `shouldBe`
    Right (Instruction 0 (Just "seti") 1 5 (Register 3))
