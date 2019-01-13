{-# LANGUAGE OverloadedStrings #-}

module Day16 where

import           Control.Applicative ((<|>))
import           Data.Foldable       (foldl')
import           Data.IntMap.Strict  (IntMap, (!))
import qualified Data.IntMap.Strict  as IntMap
import qualified Data.Set            as Set
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Day16.Operation     as Op
import           Day16.Parser
import           Day16.Types
import           Types               hiding (Input)

makeOpCodeMap :: [Sample] -> Maybe (IntMap OpName)
makeOpCodeMap samples =
  let (opCodeMap', _) = foldr f (IntMap.empty, Set.empty) samples
   in if IntMap.size opCodeMap' /= length Op.allNames
        then Nothing
        else Just opCodeMap'
  where
    f sample@(_, Instruction opCode _ _ _, _) acc@(opCodeMap, known)
      | IntMap.member opCode opCodeMap = acc
      | otherwise = maybe acc checkOps $ runSample sample
      where
        checkOps = makeNewAcc . filter (not . flip Set.member known . Op.name)
        makeNewAcc unknownOpNames
          | length unknownOpNames /= 1 = acc
          | otherwise =
            let newOpName = Op.name $ head unknownOpNames
             in ( IntMap.insert opCode newOpName opCodeMap
                , Set.insert newOpName known)

p2 :: [Sample] -> [Instruction] -> Maybe Registers
p2 samples instructions = do
  opCodeMap <- makeOpCodeMap samples
  ops <-
    traverse
      (\ins@(Instruction opCode _ _ _) ->
         Op.fromInstruction ins $ opCodeMap ! opCode)
      instructions
  return $ foldl' Op.run (IntMap.fromList [(0, 0), (1, 0), (2, 0), (3, 0)]) ops

runSample :: Sample -> Maybe [Operation]
runSample (before, instruction, after) = do
  ops <- traverse (Op.fromInstruction instruction) Op.allNames
  let results = map (Op.run before) ops
  return . map snd . filter ((==) after . fst) $ zip results ops

p1 :: [Sample] -> Maybe Int
p1 samples = do
  ops <- traverse runSample samples
  return . length $ filter (flip (>=) 3 . length) ops

run :: Text -> Either ErrMsg Text
run t = do
  (samples, instructions) <- parseInput t
  let p1result = p1 samples
  let p2result = p2 samples instructions
  return $ Text.pack ("p1: " ++ show p1result ++ " p2:" ++ show p2result)

prog :: DayProg
prog = DayProg "day16" Day16.run
