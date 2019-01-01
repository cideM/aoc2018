{-# LANGUAGE OverloadedStrings #-}

module Day16 where

import           Control.Applicative      ((<|>))
import           Control.Monad.State.Lazy (State)
import qualified Control.Monad.State.Lazy as State
import           Data.Bits                ((.&.), (.|.))
import           Data.IntMap.Strict       (IntMap, (!))
import qualified Data.IntMap.Strict       as IntMap
import qualified Data.List                as List
import qualified Data.List.Split          as Split
import           Data.Text                (Text)
import qualified Data.Text                as Text
import           Text.Parser.LookAhead    (lookAhead)
import qualified Text.Parser.Token        as Token
import           Text.Trifecta            (Parser, Result (Failure, Success))
import qualified Text.Trifecta            as Tri
import           Types                    hiding (Input)

type Registers = IntMap Int

newtype Register =
  Register Int
  deriving (Show)

newtype Immediate =
  Immediate Int
  deriving (Show)

type OpCode = Int

data Instruction =
  Instruction OpCode
              Int
              Int
              Register
  deriving (Show)

data Action
  = ADDR Register
         Register
         Register
  | ADDI Register
         Immediate
         Register
  | MULR Register
         Register
         Register
  | MULI Register
         Immediate
         Register
  | BANR Register
         Register
         Register
  | BANI Register
         Immediate
         Register
  | BORR Register
         Register
         Register
  | BORI Register
         Immediate
         Register
  | SETR Register
         Register
  | SETI Immediate
         Register
  | GTIR Immediate
         Register
         Register
  | GTRI Register
         Immediate
         Register
  | GTRR Register
         Register
         Register
  | EQRI Immediate
         Register
         Register
  | EQIR Register
         Immediate
         Register
  | EQRR Register
         Register
         Register

type Sample = (Registers, Instruction, Registers)

registersP :: Parser Registers
registersP = do
  _ <- Tri.string "Before:" <|> Tri.string "After:"
  _ <- Tri.spaces
  _ <- Tri.char '['
  contents <- Tri.decimal `Tri.sepBy` Tri.symbol ","
  _ <- Tri.char ']'
  return . IntMap.fromList . zip [0 ..] $ map fromIntegral contents

instructionP :: Parser Instruction
instructionP = do
  opCode <- dec
  x <- dec
  y <- dec
  Instruction opCode x y . Register <$> dec
  where
    dec = fromIntegral <$> (Tri.spaces *> Tri.decimal)

inputTripletP :: Parser Sample
inputTripletP = do
  before <- registersP <* Tri.spaces
  instruction <- instructionP <* Tri.spaces
  after <- registersP <* Tri.spaces
  return (before, instruction, after)

inputP :: Parser ([Sample], [Instruction])
inputP = do
  p1 <-
    Tri.manyTill inputTripletP (Tri.try . lookAhead $ Tri.count 2 instructionP)
  p2 <- Tri.many $ instructionP <* Tri.spaces
  return (p1, p2)

actionCreatorFromAction :: Action -> (Instruction -> Action)
actionCreatorFromAction action =
  case action of
    ADDR {} ->
      \(Instruction opCode a b reg) -> ADDR (Register a) (Register b) reg
    ADDI {} ->
      \(Instruction opCode a b reg) -> ADDI (Register a) (Immediate b) reg
    MULR {} ->
      \(Instruction opCode a b reg) -> MULR (Register a) (Register b) reg
    MULI {} ->
      \(Instruction opCode a b reg) -> MULI (Register a) (Immediate b) reg
    BANR {} ->
      \(Instruction opCode a b reg) -> BANR (Register a) (Register b) reg
    BANI {} ->
      \(Instruction opCode a b reg) -> BANI (Register a) (Immediate b) reg
    BORR {} ->
      \(Instruction opCode a b reg) -> BORR (Register a) (Register b) reg
    BORI {} ->
      \(Instruction opCode a b reg) -> BORI (Register a) (Immediate b) reg
    SETR {} -> \(Instruction opCode a _ reg) -> SETR (Register a) reg
    SETI {} -> \(Instruction opCode a _ reg) -> SETI (Immediate a) reg
    GTIR {} ->
      \(Instruction opCode a b reg) -> GTIR (Immediate a) (Register b) reg
    GTRI {} ->
      \(Instruction opCode a b reg) -> GTRI (Register a) (Immediate b) reg
    GTRR {} ->
      \(Instruction opCode a b reg) -> GTRR (Register a) (Register b) reg
    EQRI {} ->
      \(Instruction opCode a b reg) -> EQRI (Immediate a) (Register b) reg
    EQIR {} ->
      \(Instruction opCode a b reg) -> EQIR (Register a) (Immediate b) reg
    EQRR {} ->
      \(Instruction opCode a b reg) -> EQRR (Register a) (Register b) reg

makeActionsFromInstruction :: Instruction -> [Action]
makeActionsFromInstruction (Instruction opCode a b reg) =
  [ ADDR (Register a) (Register b) reg
  , ADDI (Register a) (Immediate b) reg
  , MULR (Register a) (Register b) reg
  , MULI (Register a) (Immediate b) reg
  , BANR (Register a) (Register b) reg
  , BANI (Register a) (Immediate b) reg
  , BORR (Register a) (Register b) reg
  , BORI (Register a) (Immediate b) reg
  , SETR (Register a) reg
  , SETI (Immediate a) reg
  , GTIR (Immediate a) (Register b) reg
  , GTRI (Register a) (Immediate b) reg
  , GTRR (Register a) (Register b) reg
  , EQRI (Immediate a) (Register b) reg
  , EQIR (Register a) (Immediate b) reg
  , EQRR (Register a) (Register b) reg
  ]

-- Iterate over samples. Map sample to actions. Evalute actions, keep the ones
-- that match after. Turn resulting lists into sets. Store op code with sets in
-- map that we're folding over (append to existing sets).
-- Iterate over map. Find common action in set for each op code.
-- Match action data constructor, and turn the one set entry into a function
-- returning that specfic action. Then use that map to fold over the
-- program
p2 :: [Sample] -> [Instruction] -> Registers
p2 samples instructions = undefined

runSample :: (Registers, Instruction, Registers) -> [Action]
runSample (before, instruction@(Instruction opCode a b reg), after) =
  let actions = makeActionsFromInstruction instruction
      results = map (runAction before) actions
   in map snd . filter ((==) after . fst) $ zip results actions

p1 :: [Sample] -> Int
p1 = length . filter (\x -> length x >= 3) . map runSample

parseInput :: Text -> Either ErrMsg ([Sample], [Instruction])
parseInput input =
  case Tri.parseString inputP mempty str of
    Failure err -> Left . Text.pack $ show err
    Success res -> Right res
  where
    str = Text.unpack input

runAction :: Registers -> Action -> Registers
runAction regs action =
  case action of
    ADDR (Register regA) (Register regB) (Register regC) ->
      let x = regs ! regA
          y = regs ! regB
       in insert regC (x + y)
    ADDI (Register regA) (Immediate y) (Register regC) ->
      let x = regs ! regA
       in insert regC (x + y)
    MULR (Register regA) (Register regB) (Register regC) ->
      let x = regs ! regA
          y = regs ! regB
       in insert regC (x * y)
    MULI (Register regA) (Immediate y) (Register regC) ->
      let x = regs ! regA
       in insert regC (x * y)
    BANR (Register regA) (Register regB) (Register regC) ->
      let x = regs ! regA
          y = regs ! regB
       in insert regC (x .&. y)
    BANI (Register regA) (Immediate y) (Register regC) ->
      let x = regs ! regA
       in insert regC (x .&. y)
    BORR (Register regA) (Register regB) (Register regC) ->
      let x = regs ! regA
          y = regs ! regB
       in insert regC (x .|. y)
    BORI (Register regA) (Immediate y) (Register regC) ->
      let x = regs ! regA
       in insert regC (x .|. y)
    SETR (Register regA) (Register regC) -> insert regC (regs ! regA)
    SETI (Immediate x) (Register regC) -> insert regC x
    GTIR (Immediate x) (Register regB) (Register regC) ->
      let y = regs ! regB
          v =
            if x > y
              then 1
              else 0
       in insert regC v
    GTRI (Register regA) (Immediate y) (Register regC) ->
      let x = regs ! regA
          v =
            if x > y
              then 1
              else 0
       in insert regC v
    GTRR (Register regA) (Register regB) (Register regC) ->
      let x = regs ! regA
          y = regs ! regB
          v =
            if x > y
              then 1
              else 0
       in insert regC v
    EQRI (Immediate x) (Register regB) (Register regC) ->
      let y = regs ! regB
          v =
            if x == y
              then 1
              else 0
       in insert regC v
    EQIR (Register regA) (Immediate y) (Register regC) ->
      let x = regs ! regA
          v =
            if x == y
              then 1
              else 0
       in insert regC v
    EQRR (Register regA) (Register regB) (Register regC) ->
      let x = regs ! regA
          y = regs ! regB
          v =
            if x == y
              then 1
              else 0
       in insert regC v
  where
    insert k v = IntMap.insert k v regs

run :: Text -> Either ErrMsg Text
run t = do
  (p1input, _) <- parseInput t
  return . Text.pack . show $ p1 p1input
  where
    parsed = parseInput t

prog :: DayProg
prog = DayProg "day16" run
