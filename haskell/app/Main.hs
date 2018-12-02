{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Day1
import           Types
import           Data.Map.Strict               as Map'
import           Data.Text                     as Text
import           Prelude                 hiding ( FilePath
                                                , lookup
                                                )
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

dayP :: Parser DayNum
dayP = strOption
  (long "day" <> short 'd' <> metavar "DAYNUMBER" <> help "Advent of Code Day")

opts :: Parser Options
opts = Options <$> inputP <*> dayP

days :: Map DayNum DayProg
days = Map'.fromList [("1", Day1.prog)]

readDataFromFile :: FilePath -> IO Text
readDataFromFile fp = TextIO.readFile $ Text.unpack fp

readDataFromStdIn :: IO Text
readDataFromStdIn = TextIO.getContents

readData :: Options -> IO Text
readData (Options (FileInput p) _) = readDataFromFile p
readData (Options StdIn         _) = readDataFromStdIn

getDayProg :: Options -> Maybe DayProg
getDayProg (Options _ n) = lookup n days

main :: IO ()
main = do
  opts <- execParser program
  case getDayProg opts of
    Nothing -> TextIO.putStrLn "No program found for that day!"
    Just p  -> do
      d <- readData opts
      case _run p d of
        (Left  e) -> TextIO.putStrLn e
        (Right r) -> TextIO.putStrLn . Text.pack $ show r
 where
  program = info
    (helper <*> opts)
    (fullDesc <> header "Merry Wintersday :)" <> progDesc
      "I miss Guild Wars 2 :("
    )
