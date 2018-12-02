module Types where

import           Data.Text
import           Prelude                 hiding ( FilePath )

type ErrMsg = Text

type FilePath = Text

data Input = FileInput !FilePath | StdIn

newtype Options = Options {
                       input :: Input
                       }
