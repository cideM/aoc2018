#!/usr/bin/env stack
{-
    stack
    script
    --resolver lts-12.20
    --package text,trifecta,containers,parsers
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Day16 where

import           Control.Applicative   ((<|>))
import           Data.Bits             ((.&.), (.|.))
import           Data.Foldable         (foldl')
import           Data.IntMap.Strict    (IntMap, (!))
import qualified Data.IntMap.Strict    as IntMap
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import qualified Data.Set              as Set
import           Data.Text             (Text)
import           Text.Parser.LookAhead (lookAhead)
import           Text.Trifecta         (Parser, Result (Failure, Success))
import qualified Text.Trifecta         as Tri

type Registers = IntMap Int

newtype Register =
  Register Int
  deriving (Show, Eq, Ord)

type OpCode = Int

type OpName = Text

data Instruction = Instruction
  { a   :: Int
  , b   :: Int
  , out :: Register
  } deriving (Show, Eq, Ord)

type Sample = (Registers, InstructionWithCode, Registers)

type InstructionWithCode = (OpCode, Instruction)

type Operation = Instruction -> Registers -> Registers

registersP :: Parser Registers
registersP = do
  _ <- Tri.string "Before:" <|> Tri.string "After:"
  _ <- Tri.spaces
  _ <- Tri.char '['
  contents <- Tri.decimal `Tri.sepBy` Tri.symbol ","
  _ <- Tri.char ']'
  return . IntMap.fromList . zip [0 ..] $ map fromIntegral contents

instructionP :: Parser InstructionWithCode
instructionP = do
  opCode <- dec
  x <- dec
  y <- dec
  reg <- dec
  return (opCode, Instruction x y (Register reg))
  where
    dec = fromIntegral <$> (Tri.spaces *> Tri.decimal)

inputTripletP :: Parser Sample
inputTripletP = do
  before <- registersP <* Tri.spaces
  instructionWithCode <- instructionP <* Tri.spaces
  after <- registersP <* Tri.spaces
  return (before, instructionWithCode, after)

inputP :: Parser ([Sample], [InstructionWithCode])
inputP = do
  p1input <-
    Tri.manyTill inputTripletP (Tri.try . lookAhead $ Tri.count 2 instructionP)
  p2input <- Tri.many $ instructionP <* Tri.spaces
  return (p1input, p2input)

makeOpCodeMap :: [Sample] -> Maybe (IntMap OpName)
makeOpCodeMap samples =
  let (opCodeMap', _) = foldr f (IntMap.empty, Set.empty) samples
   in if IntMap.size opCodeMap' /= Map.size ops
        then Nothing
        else Just opCodeMap'
  where
    f sample@(_, (opCode, _), _) acc@(opCodeMap, known)
      | IntMap.member opCode opCodeMap = acc
      | otherwise =
        fn . filter (not . flip Set.member known . fst) $ runSample sample
      where
        fn unknownOpNames
          | length unknownOpNames /= 1 = acc
          | otherwise =
            let newName = fst $ head unknownOpNames
             in ( IntMap.insert opCode newName opCodeMap
                , Set.insert newName known)

p2 :: [Sample] -> [InstructionWithCode] -> Maybe Registers
p2 samples instrs = do
  opCodeMap <- makeOpCodeMap samples
  -- | for each instruction get the associated operation and partially apply to
  -- instruction
  ops' <- traverse (getOp opCodeMap) instrs
  return $
    foldl'
      (\regs op -> op regs)
      (IntMap.fromList [(0, 0), (1, 0), (2, 0), (3, 0)])
      ops'
  where
    getOp opCodeMap (opCode, ins) = do
      opName' <- IntMap.lookup opCode opCodeMap
      op <- Map.lookup opName' ops
      return $ op ins

runSample :: Sample -> [(OpName, Registers)]
runSample (before, (_, instruction), after) =
  filter ((==) after . snd) . map (\(name, op) -> (name, op instruction before)) $
  Map.assocs ops

p1 :: [Sample] -> Int
p1 = length . filter (flip (>=) 3 . length) . map runSample

ops :: Map OpName Operation
ops
  -- | Scary but just utility. The lambda functions in the list are just the
  -- value updater. There is some plumbing though, to extract the "out" int from
  -- the Register newtype. Also update the map with the new value. And that's
  -- done by "f".
 =
  Map.fromList
    [ ("addr", f (\a b rs -> rs ! a + rs ! b))
    , ("addi", f (\a b rs -> rs ! a + b))
    , ("mulr", f (\a b rs -> rs ! a * rs ! b))
    , ("muli", f (\a b rs -> rs ! a * b))
    , ("banr", f (\a b rs -> rs ! a .&. rs ! b))
    , ("bani", f (\a b rs -> rs ! a .&. b))
    , ("borr", f (\a b rs -> rs ! a .|. rs ! b))
    , ("bori", f (\a b rs -> rs ! a .|. b))
    , ("setr", f (\a _ rs -> rs ! a))
    , ("seti", f (\a _ _ -> a))
    , ( "gtir"
      , f (\a b rs ->
             if a > rs ! b
               then 1
               else 0))
    , ( "gtri"
      , f (\a b rs ->
             if rs ! a > b
               then 1
               else 0))
    , ( "gtrr"
      , f (\a b rs ->
             if rs ! a > rs ! b
               then 1
               else 0))
    , ( "eqri"
      , f (\a b rs ->
             if rs ! a == b
               then 1
               else 0))
    , ( "eqir"
      , f (\a b rs ->
             if a == rs ! b
               then 1
               else 0))
    , ( "eqrr"
      , f (\a b rs ->
             if rs ! a == rs ! b
               then 1
               else 0))
    ]
  where
    f makeNewVal Instruction {..} registers =
      let Register out' = out
       in IntMap.insert out' (makeNewVal a b registers) registers

main :: IO ()
main = do
  input <- Tri.parseString inputP mempty <$> getContents
  case input of
    Failure parseErr -> print parseErr
    Success (samples, instructions) -> do
      print $ p1 samples
      print $ p2 samples instructions
