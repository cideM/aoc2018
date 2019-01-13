{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Day16.Operation
  ( Operation
  , OpName
  , ops
  ) where

import           Data.Bits          ((.&.), (.|.))

import           Data.IntMap.Strict ((!))
import qualified Data.IntMap.Strict as IntMap
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.Text          (Text)
import           Day16.Types

type Operation = Instruction -> Registers -> Registers

type OpName = Text

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
