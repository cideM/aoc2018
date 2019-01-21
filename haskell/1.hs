#!/usr/bin/env stack
{-
    stack
    script
    --resolver lts-12.20
    --package containers,text,trifecta
-}
module Day1 where

import qualified Data.Set           as Set
import           Data.Text          (Text)
import qualified Data.Text          as Text
import           Data.Text.IO       as TIO
import           Prelude            hiding (print)
import qualified System.Environment as Env
import           Text.Trifecta      hiding (err)

firstDuplicate :: [Integer] -> Maybe Integer
firstDuplicate nums = go (Set.singleton 0) 0 . concat $ repeat nums
  where
    go _ _ [] = Nothing
    go seen acc (x:xs) =
      let x' = acc + x
       in if Set.member x' seen
            then Just x'
            else go (Set.insert x' seen) x' xs

parse :: Parser a -> Text -> Result a
parse p = parseString p mempty . Text.unpack

main :: IO ()
main = do
  args <- Env.getArgs
  input <- TIO.readFile (head args)
  mapM_ (mapM_ print . firstDuplicate) $
    parse (many (integer <* whiteSpace)) input
  where
    print = TIO.putStrLn . Text.pack . show
