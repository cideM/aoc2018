{-# LANGUAGE OverloadedStrings #-}

module Day19.Run where

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Day19.Parser                   ( parseInput )
import           Types                   hiding ( Input )

run :: Text -> Either ErrMsg Text
run text = parseInput text >>= \(ip, instructions) -> return "Foo"

prog :: DayProg
prog = DayProg "day16" run
