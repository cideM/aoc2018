module Types where

import           Data.Text
import           Prelude   hiding (FilePath)

type ErrMsg = Text

type FilePath = Text

data Input
  = FileInput !FilePath
  | CliInput !String

type DayNum = Text

type Exercise = Text -> Either ErrMsg Text

data Options = Options
  { _input  :: Input
  , _dayNum :: DayNum
  }
