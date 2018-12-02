module Types where

import           Data.Text
import           Prelude                 hiding ( FilePath )

data DayProg = DayProg {
    _name :: Text,
    _run :: Text -> Either ErrMsg Solution
                  }

type Sum = Int
type FirstRepeat = Maybe Int

data Day1Result = Day1Result !Sum !FirstRepeat

type ErrMsg = Text

type Solution = Text

type FilePath = Text

data Input = FileInput !FilePath | StdIn

type DayNum = Text

data Options = Options {
                       _input :: Input,
                       _dayNum :: DayNum
                       }
