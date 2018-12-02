{-# LANGUAGE OverloadedStrings #-}

module Day2
  ( prog
  )
where

import           Data.Text                     as Text
import           Types
import           Data.Text.Read                as Read
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set

prog :: DayProg
prog = DayProg "day2" (const . Right $ Types.pack "foo")
