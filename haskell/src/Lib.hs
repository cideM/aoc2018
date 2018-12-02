module Lib
  ( day1
  )
where

import           Data.Text                     as Text
import           Types
import           Data.Text.IO                  as TextIO
import           Data.Text.Read                as Read
import qualified System.IO                     as IO
import           Prelude                 hiding ( FilePath )

transformData :: Text -> Either ErrMsg [Int]
transformData d = case res of
  (Left  e) -> Left $ Text.pack e
  -- ^ Not sure why Data.Text.Read has a Reader with String, rather than Text
  -- Here we're just transforming Either String a into Either Text a
  (Right i) -> Right i
  where res = traverse (fmap fst . Read.signed Read.decimal) $ Text.lines d

readDataFromFile :: FilePath -> IO Text
readDataFromFile fp = TextIO.readFile $ Text.unpack fp

readDataFromStdIn :: IO Text
readDataFromStdIn = TextIO.getContents

readData :: Input -> IO Text
readData (FileInput p) = readDataFromFile p
readData StdIn         = readDataFromStdIn

day1 :: Options -> IO (Either ErrMsg Int)
day1 (Options input) = fmap sum . transformData <$> readData input
