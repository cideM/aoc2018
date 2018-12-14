module Types where

import Data.Text
import Prelude hiding (FilePath)

data DayProg = DayProg
  { _name :: Text
  , _run :: Text -> Either ErrMsg Text
  }

type ErrMsg = Text

type FilePath = Text

data Input
  = FileInput !FilePath
  | CliInput !String

type DayNum = Text

data Options = Options
  { _input :: Input
  , _dayNum :: DayNum
  }
