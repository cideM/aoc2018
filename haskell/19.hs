#!/usr/bin/env stack
{-
    stack
    script
    --resolver lts-12.20
    --package text,trifecta,containers,parsers,mtl,vector
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import           Data.Bits     ((.&.), (.|.))
import           Data.IntMap   (IntMap, (!))
import qualified Data.IntMap   as IntMap
import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.String   (IsString)
import           Data.Text     (Text)
import qualified Data.Text     as Text
import           Data.Vector   (Vector)
import qualified Data.Vector   as Vector
import           Text.Trifecta (Parser, Result (Failure, Success))
import qualified Text.Trifecta as Tri

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

type InstructionWithName = (OpName, Instruction)

-- | Instruction Pointer
data IP = IP
  { register :: Register
  , value    :: Int
  } deriving (Show, Eq)

type Input = (IP, Vector InstructionWithName)

-- | Maybe to Either.
mb2E :: (IsString a) => a -> Maybe b -> Either a b
mb2E = flip maybe Right . Left

instructionPointerP :: Parser IP
instructionPointerP =
  IP <$>
  (Tri.string "#ip" *> Tri.whiteSpace *>
   (Register . fromIntegral <$> Tri.natural)) <*>
  pure 0

instructionP :: Parser InstructionWithName
instructionP =
  (,) <$> opName' <*>
  (Instruction <$> number' <*> number' <*> (Register <$> number'))
  -- | The opCode is not really 0 but I don't feel like refactoring everything
  -- from OpCode to Maybe OpCode.
  where
    number' = fromIntegral <$> Tri.natural <* Tri.whiteSpace
    opName' =
      Text.concat . map Text.singleton <$> Tri.many Tri.letter <* Tri.whiteSpace

inputP :: Parser (IP, Vector InstructionWithName)
inputP =
  (,) <$> instructionPointerP <* Tri.whiteSpace <*>
  (Vector.fromList <$> Tri.many instructionP)

parseInput :: Text -> Either Text (IP, Vector InstructionWithName)
parseInput input =
  case Tri.parseString inputP mempty $ Text.unpack input of
    Failure err -> Left . Text.pack $ show err
    Success res -> Right res

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

runIp ::
     IP
  -> Registers
  -> Vector InstructionWithName
  -> Either Text (IP, Registers)
runIp ip@IP {..} regs instrs = do
  let (opName, instr) = (Vector.!) instrs value -- ^ Get instruction at index (instruction pointer value)
      Register r = register
      withIpValue = IntMap.insert r value regs -- ^ Insert ip value into register
  op <- mb2E ("Unkown op " <> opName) (Map.lookup opName ops)
  let regs' = op instr withIpValue
      value' = (regs' ! r) + 1
  if value' < 0 || value' > Vector.length instrs - 1
    then Right (ip, regs')
    else runIp (IP (Register r) value') regs' instrs

main :: IO ()
main = do
  input <- Tri.parseString inputP mempty <$> getContents
  case input of
    Failure parseErr -> print parseErr
    Success (ip, instructions) ->
      mapM_
        (uncurry printResult)
        (runIp
           ip
           (IntMap.fromList [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0)])
           instructions)
  where
    printResult ip registers = do
      putStr "Instruction pointer: "
      print ip
      putStr "Registers: "
      print registers
