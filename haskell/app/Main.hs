{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Map.Strict     as Map'
import           Data.Semigroup      ((<>))
import           Data.Text           as Text
import qualified Data.Text.IO        as TextIO
import qualified Day1
import qualified Day10
import qualified Day13
import qualified Day15
import qualified Day16
import qualified Day19.Run
import qualified Day20.Run
import qualified Day2
import qualified Day3
import qualified Day8
import qualified Day9
import qualified Day11
import           Options.Applicative
import           Prelude             hiding (FilePath, lookup)
import           Types

fileInputP :: Parser Input
fileInputP =
  FileInput <$>
  strOption
    (long "file" <> short 'f' <> metavar "FILENAME" <> help "Path to input file")

cliInputP :: Parser Input
cliInputP =
  CliInput <$>
  strOption
    (long "data" <> metavar "CLIINPUT" <> help "Pass data in via the CLI")

inputP :: Parser Input
inputP = fileInputP <|> cliInputP

dayP :: Parser DayNum
dayP =
  strOption
    (long "day" <> short 'd' <> metavar "DAYNUMBER" <> help "Advent of Code Day")

opts :: Parser Options
opts = Options <$> inputP <*> dayP

days :: Map DayNum Exercise
days =
  Map'.fromList
    [ ("1", Day1.run)
    , ("2", Day2.run)
    , ("3", Day3.run)
    , ("8", Day8.run)
    , ("9", Day9.run)
    , ("10", Day10.run)
    , ("11", Day11.run)
    , ("13", Day13.run)
    , ("15", Day15.run)
    , ("16", Day16.run)
    , ("19", Day19.Run.run)
    , ("20", Day20.Run.run)
    ]

readData :: Options -> IO Text
readData (Options (FileInput fp) _)     = TextIO.readFile $ Text.unpack fp
readData (Options (CliInput cliData) _) = return $ Text.pack cliData

getDayProg :: Options -> Maybe Exercise
getDayProg (Options _ n) = lookup n days

main :: IO ()
main = do
  opts <- execParser program
  case getDayProg opts of
    Nothing -> TextIO.putStrLn "No program found for that day!"
    Just p -> do
      d <- readData opts
      case p d of
        (Left e)  -> TextIO.putStrLn e
        (Right r) -> TextIO.putStrLn r
  where
    program =
      info
        (helper <*> opts)
        (fullDesc <> header "Merry Wintersday :)" <>
         progDesc "I miss Guild Wars 2 :(")
