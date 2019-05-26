{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad.Loops      (whileM)
import qualified Control.Monad.State.Lazy as S
import qualified Data.Map.Strict          as M'
import qualified Data.Text                as T
import           Lib
import           Parser
import           Text.Megaparsec          hiding (State)
import           Text.Pretty.Simple       (pPrint)
import           Types

main :: IO ()
main = do
  parsed <- runParser inputParser "stdin" . T.pack <$> getContents
  case parsed of
    Left e -> pPrint e
    Right s -> do
      let s' = S.execState (whileM (not <$> hasWinner) fight) $ M'.unions s
      pPrint s'
      pPrint . sum . map _unitCount $ M'.elems s'
      let boost =
            M'.map
              (\group ->
                 if _team group == TeamName "Immune System"
                   then group {_damage = _damage group + 79}
                   else group) $
            M'.unions s
      let s'' = S.execState (whileM (not <$> hasWinner) fight) boost
      pPrint s''
      pPrint . sum . map _unitCount $ M'.elems s''
