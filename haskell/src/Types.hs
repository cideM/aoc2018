{-# LANGUAGE ExistentialQuantification #-}

module Types where

import           Data.Text
import           Prelude                 hiding ( FilePath )

-- https://wiki.haskell.org/Heterogenous_collections
-- An existential type encapsulating types that can be Shown
-- The interface to the type is held in the show method dictionary
--
-- Create your own typeclass for packing up other interfaces
data Showable = forall a . Show a => MkShowable a

pack :: Show a => a -> Showable
pack = MkShowable

data DayProg = DayProg {
    _name :: Text,
    _run :: Text -> Either ErrMsg Showable
                  }

type Sum = Int

type FirstRepeat = Maybe Int

data Day1Result = Day1Result !Sum !FirstRepeat deriving (Show)

type ErrMsg = Text

type Solution = Text

type FilePath = Text

data Input = FileInput !FilePath | StdIn

type DayNum = Text

data Options = Options {
                       _input :: Input,
                       _dayNum :: DayNum
                       }
