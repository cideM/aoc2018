{-# LANGUAGE OverloadedStrings #-}

module Day19.Run where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Types     hiding (Input)

-- Do I really need the value in the IP? It's always in the registers too
-- TODO: InstructionPointer.run :: IP -> Vector Instruction -> Registers -> (IP, Registers)
-- Get instruction register from IP. Get value of IP from registers (?). Get
-- instruction based on IP value. Run instruction/operation. Copy IP registers
-- to IP value. Increment by 1.
run :: Text -> Either ErrMsg Text
run _ = Right "Foo"

prog :: DayProg
prog = DayProg "day16" run
