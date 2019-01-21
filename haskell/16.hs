{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Day16 (run) where

import           Data.Foldable      (foldl')
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict    as Map
import qualified Data.Set           as Set
import           Data.Text          (Text)
import qualified Data.Text          as Text
import qualified Day16.Operation    as Op
import           Day16.Parser
import           Day16.Types
import           Types              hiding (Input)

makeOpCodeMap :: [Sample] -> Maybe (IntMap OpName)
makeOpCodeMap samples =
  let (opCodeMap', _) = foldr f (IntMap.empty, Set.empty) samples
   in if IntMap.size opCodeMap' /= Map.size Op.ops
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
      op <- Map.lookup opName' Op.ops
      return $ op ins

runSample :: Sample -> [(OpName, Registers)]
runSample (before, (_, instruction), after) =
  filter ((==) after . snd) . map (\(name, op) -> (name, op instruction before)) $
  Map.assocs Op.ops

p1 :: [Sample] -> Int
p1 = length . filter (flip (>=) 3 . length) . map runSample

run :: Text -> Either ErrMsg Text
run t = do
  (samples, instructions) <- parseInput t
  let p1result = p1 samples
  let p2result = p2 samples instructions
  return $ Text.pack ("p1: " ++ show p1result ++ " p2:" ++ show p2result)

