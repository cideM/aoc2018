{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Day16 where

import           Control.Applicative   ((<|>))
import           Data.Bits             ((.&.), (.|.))
import           Data.Foldable         (foldl')
import           Data.IntMap.Strict    (IntMap, (!))
import qualified Data.IntMap.Strict    as IntMap
import qualified Data.Set              as Set
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           Text.Parser.LookAhead (lookAhead)
import           Text.Trifecta         (Parser, Result (Failure, Success))
import qualified Text.Trifecta         as Tri
import           Types                 hiding (Input)

type Registers = IntMap Int

newtype Register =
  Register Int
  deriving (Show, Eq, Ord)

newtype Immediate =
  Immediate Int
  deriving (Show, Eq, Ord)

type OpCode = Int

data Instruction =
  Instruction OpCode
              Int
              Int
              Register
  deriving (Show, Eq, Ord)

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

type Sample = (Registers, Instruction, Registers)

type OpName = Text

data Operation = Operation
  { output :: Register
  , kind   :: OpKind
  , name   :: OpName
  } deriving (Ord, Eq, Show)

opFromIns :: Instruction -> OpName -> Maybe Operation
opFromIns (Instruction _ inputA inputB output) opName = do
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

opNames :: [Text]
opNames =
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

makeOpCodeMap :: [Sample] -> Maybe (IntMap OpName)
makeOpCodeMap samples =
  let (opCodeMap', _) = foldr f (IntMap.empty, Set.empty) samples
   in if IntMap.size opCodeMap' /= length opNames
        then Nothing
        else Just opCodeMap'
  where
    f (before, ins@(Instruction opCode _ _ _), after) acc@(opCodeMap, known)
      | IntMap.member opCode opCodeMap = acc
      | otherwise = maybe acc checkOps $ traverse (opFromIns ins) opNames
      where
        checkOps ops =
          let results = map (runOp before) ops
              matchingBeforeAfter =
                map snd . filter ((==) after . fst) $ zip results ops
              unknown =
                filter (not . flip Set.member known . name) matchingBeforeAfter
           in if length unknown == 1
                then let newOpName = name $ head unknown
                      in ( IntMap.insert opCode newOpName opCodeMap
                         , Set.insert newOpName known)
                else acc

p2 :: [Sample] -> [Instruction] -> Maybe Registers
p2 samples instructions = do
  opCodeMap <- makeOpCodeMap samples
  ops <-
    traverse
      (\ins@(Instruction opCode _ _ _) -> opFromIns ins $ opCodeMap ! opCode)
      instructions
  return $ foldl' runOp (IntMap.fromList [(0, 0), (1, 0), (2, 0), (3, 0)]) ops

runSample :: Sample -> Maybe [Operation]
runSample (before, instruction, after) = do
  ops <- traverse (opFromIns instruction) opNames
  let results = map (runOp before) ops
  return . map snd . filter ((==) after . fst) $ zip results ops

p1 :: [Sample] -> Maybe Int
p1 samples = do
  ops <- traverse runSample samples
  return . length $ filter (flip (>=) 3 . length) ops

parseInput :: Text -> Either ErrMsg ([Sample], [Instruction])
parseInput input =
  case Tri.parseString inputP mempty str of
    Failure err -> Left . Text.pack $ show err
    Success res -> Right res
  where
    str = Text.unpack input

runOp :: Registers -> Operation -> Registers
runOp regs Operation {..} =
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

run :: Text -> Either ErrMsg Text
run t = do
  (samples, instructions) <- parseInput t
  let p1result = p1 samples
  let p2result = p2 samples instructions
  return $ Text.pack ("p1: " ++ show p1result ++ " p2:" ++ show p2result)

prog :: DayProg
prog = DayProg "day16" run
