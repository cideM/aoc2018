#!/usr/bin/env stack
{-
    stack
    script
    --resolver lts-12.20
    --package containers,trifecta
-}
{-# LANGUAGE RecordWildCards #-}

module Day3 where

import qualified Data.List       as List
import qualified Data.Map.Strict as Map
import           Text.Trifecta

type Coords = (Int, Int) -- ^ x y

data Claim = Claim
  { _claimID :: !Int
  , _origin  :: Coords
  , _width   :: !Int
  , _height  :: !Int
  } deriving (Show, Eq)

claimP :: Parser Claim
claimP = Claim <$> claimId <*> origin <*> width <*> height
  where
    claimId = char '#' *> (int <* whiteSpace)
    origin = (,) <$> (char '@' *> whiteSpace *> int) <*> (char ',' *> int)
    width = char ':' *> whiteSpace *> int
    height = char 'x' *> int <* whiteSpace
    int = fromIntegral <$> integer

getCoords :: Claim -> [Coords]
getCoords Claim {..} =
  let (x0, y0) = _origin
   in do x <- [x0 .. x0 + _width - 1]
         y <- [y0 .. y0 + _height - 1]
         return (x, y)

main :: IO ()
main = do
  input <- getContents
  case parseString (many $ claimP <* whiteSpace) mempty input of
    Failure cause -> print cause
    Success claims -> do
      let withCoords = map (\c -> (c, getCoords c)) claims
          -- ^ For each claim, generate its coordinates
          claimsMap =
            foldr (Map.unionWith (+) . toMap . snd) Map.empty withCoords
            -- ^ Merge the coords of all claims into a map counting how often a
            -- certain point is claimed.
          overlap = Map.filter (>= 2) claimsMap
          -- ^ Keep only overlaps
      -- | Part 1: Size of overlap map
      print . Map.size $ overlap
      -- | Part 2: Find the claim whose coordinate map has 0 intersection with
      -- overlap map
      mapM_ (print . _claimID . fst) $
        List.find
          ((==) 0 . Map.size . Map.intersection overlap . toMap . snd)
          withCoords
  where
    toMap = Map.fromList . flip zip ([1,1 ..] :: [Integer])
