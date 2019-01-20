{-# LANGUAGE OverloadedStrings #-}

module Day19.Run (run) where

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Day19.Parser                   ( parseInput )
import qualified Day19.InstructionPointer      as IP
import           Types                   hiding ( Input )
import qualified Data.IntMap                   as IntMap

run :: Text -> Either ErrMsg Text
run text = parseInput text >>= \(ip, instructions) -> do
    (ip', registers) <- IP.run
        ip
        (IntMap.fromList [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0)])
        instructions
    let p1 =
            "Instruction pointer: "
                <> Text.pack (show ip')
                <> " Registers: "
                <> Text.pack (show registers)

    (ip2, registers2) <- IP.run
        ip
        (IntMap.fromList [(0, 1), (1, 0), (2, 0), (3, 0), (4, 0), (5, 0)])
        instructions
    let p2 =
            "Instruction pointer: "
                <> Text.pack (show ip2)
                <> " Registers: "
                <> Text.pack (show registers2)
    return $ p1 <> " " <> p2

