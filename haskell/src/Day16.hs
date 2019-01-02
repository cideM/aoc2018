{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module Day16 where

import           Control.Applicative   ((<|>))
import           Data.Bits             ((.&.), (.|.))
import           Data.Data             (Data)
import qualified Data.Data             as Data
import           Data.IntMap.Strict    (IntMap, (!))
import qualified Data.IntMap.Strict    as IntMap
import qualified Data.List             as List
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Data.Typeable         (Typeable)
import           Text.Parser.LookAhead (lookAhead)
import           Text.Trifecta         (Parser, Result (Failure, Success))
import qualified Text.Trifecta         as Tri
import           Types                 hiding (Input)

type Registers = IntMap Int

newtype Register =
  Register Int
  deriving (Show, Eq, Ord, Data, Typeable)

newtype Immediate =
  Immediate Int
  deriving (Show, Eq, Ord, Data)

type OpCode = Int

data Instruction =
  Instruction OpCode
              Int
              Int
              Register
  deriving (Show, Eq, Ord)

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
  deriving (Ord, Eq, Show, Data, Typeable)

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
  p1input <-
    Tri.manyTill inputTripletP (Tri.try . lookAhead $ Tri.count 2 instructionP)
  p2input <- Tri.many $ instructionP <* Tri.spaces
  return (p1input, p2input)

actionCreatorFromConstrString :: String -> (Instruction -> Action)
actionCreatorFromConstrString consStr =
  case consStr of
    "ADDR" -> \(Instruction _ a b reg) -> ADDR (Register a) (Register b) reg
    "ADDI" -> \(Instruction _ a b reg) -> ADDI (Register a) (Immediate b) reg
    "MULR" -> \(Instruction _ a b reg) -> MULR (Register a) (Register b) reg
    "MULI" -> \(Instruction _ a b reg) -> MULI (Register a) (Immediate b) reg
    "BANR" -> \(Instruction _ a b reg) -> BANR (Register a) (Register b) reg
    "BANI" -> \(Instruction _ a b reg) -> BANI (Register a) (Immediate b) reg
    "BORR" -> \(Instruction _ a b reg) -> BORR (Register a) (Register b) reg
    "BORI" -> \(Instruction _ a b reg) -> BORI (Register a) (Immediate b) reg
    "SETR" -> \(Instruction _ a _ reg) -> SETR (Register a) reg
    "SETI" -> \(Instruction _ a _ reg) -> SETI (Immediate a) reg
    "GTIR" -> \(Instruction _ a b reg) -> GTIR (Immediate a) (Register b) reg
    "GTRI" -> \(Instruction _ a b reg) -> GTRI (Register a) (Immediate b) reg
    "GTRR" -> \(Instruction _ a b reg) -> GTRR (Register a) (Register b) reg
    "EQRI" -> \(Instruction _ a b reg) -> EQRI (Immediate a) (Register b) reg
    "EQIR" -> \(Instruction _ a b reg) -> EQIR (Register a) (Immediate b) reg
    "EQRR" -> \(Instruction _ a b reg) -> EQRR (Register a) (Register b) reg

makeActionsFromInstruction :: Instruction -> [Action]
makeActionsFromInstruction (Instruction _ a b reg) =
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

opCodeToConstrStr :: [Sample] -> Maybe (IntMap String)
opCodeToConstrStr samples =
  let opCodeMap = foldl buildMap IntMap.empty samples
   in narrowMap opCodeMap IntMap.empty
  where
    buildMap acc sample@(_, Instruction opCode _ _ _, _) =
      let actions =
            Set.fromList . map (Data.showConstr . Data.toConstr) $
            runSample sample
       in IntMap.insertWith Set.intersection opCode actions acc
    narrowMap :: IntMap (Set String) -> IntMap String -> Maybe (IntMap String)
    narrowMap mapIn mapOut
      | IntMap.size mapIn == 0 = Just mapOut
      | otherwise =
        let solved = List.find ((==) 1 . length . snd) $ IntMap.assocs mapIn
         in case solved of
              Nothing -> Nothing
              Just (k, v) ->
                let constrStr = head $ Set.toList v
                    mapOut' = IntMap.insert k constrStr mapOut
                    mapIn' =
                      IntMap.delete k (IntMap.map (Set.delete constrStr) mapIn)
                 in narrowMap mapIn' mapOut'

p2 :: [Sample] -> [Instruction] -> Maybe Registers
p2 samples instructions = do
  opCodeMap <- opCodeToConstrStr samples
  return $
    foldl
      (runProgram opCodeMap)
      (IntMap.fromList [(0, 0), (1, 0), (2, 0), (3, 0)])
      instructions
  where
    runProgram opCodeMap acc ins@(Instruction opCode _ _ _) =
      let actionCreator = actionCreatorFromConstrString $ opCodeMap ! opCode
       in runAction acc (actionCreator ins)

runSample :: Sample -> [Action]
runSample (before, instruction, after) =
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
  (samples, instructions) <- parseInput t
  let p1result = p1 samples
  let p2result = p2 samples instructions
  return $ Text.pack ("p1: " ++ show p1result ++ " p2:" ++ show p2result)

prog :: DayProg
prog = DayProg "day16" run
