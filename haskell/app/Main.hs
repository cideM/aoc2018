{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Lib
import           Types
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as TextIO
import           Options.Applicative
import           Data.Semigroup                 ( (<>) )

fileInputP :: Parser Input
fileInputP = FileInput <$> strOption
  (long "file" <> short 'f' <> metavar "FILENAME" <> help "Path to input file")

stdInP :: Parser Input
stdInP = flag' StdIn (long "stdin" <> help "Read from stdin")

inputP :: Parser Input
inputP = fileInputP <|> stdInP

opts :: Parser Options
opts = Options <$> inputP

main :: IO ()
main =
  execParser program
    >>= Lib.day1
    >>= \case
      (Left e) -> TextIO.putStrLn e
      (Right r) -> TextIO.putStrLn . Text.pack $ show r
 where
  program = info
    (helper <*> opts)
    (fullDesc <> header "Advent of Code - Day 1" <> progDesc
      "Adjust frequency abberrations"
    )
