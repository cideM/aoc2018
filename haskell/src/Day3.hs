{-# LANGUAGE OverloadedStrings #-}

module Day3 where

import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Parser.Char
import Text.Trifecta
import Types

type ID = Text

data Claim = Claim
  { id :: ID
  , left :: Int
  , top :: Int
  , width :: Int
  , height :: Int
  } deriving (Show, Eq)

data Point =
  Point !Int
        !Int
  deriving (Show, Ord, Eq)

type Count = Int

newtype Overlap =
  Overlap Int
  deriving (Show, Eq, Ord)

data Day2Result = Day2Result
  { claimWithoutOverlap :: Claim
  , overlappingInches :: Int
  } deriving (Show)

idP :: Parser ID
idP = Text.pack <$> (spaces *> char '#' *> some digit <* spaces)

-- #123 @ 3,2: 5x4
claimP :: Parser Claim
claimP =
  Claim <$> idP <* char '@' <* spaces <*> (read <$> some digit) <* char ',' <*>
  (read <$> some digit) <*
  char ':' <*
  spaces <*>
  (read <$> some digit) <*
  char 'x' <*>
  (read <$> some digit)

claim2Points :: Claim -> [Point]
claim2Points c =
  [ Point x y
  | x <- [left c + 1 .. left c + width c]
  , y <- [top c + 1 .. top c + height c]
  ]

trackOverlap :: [Point] -> Map Point Count
trackOverlap = List.foldr f Map.empty
  where
    f p m =
      case Map.lookup p m of
        (Just v) -> Map.insert p (v + 1) m
        Nothing -> Map.insert p 1 m

notOverlapping :: [Claim] -> Maybe Claim
notOverlapping cs = List.find overlapsNothing cs
  where
    overlapsNothing c =
      let without = List.delete c cs
       in List.all Maybe.isNothing $ fmap (claimOverlap c) without

run :: Text -> Either ErrMsg Text
run t =
  case claims of
    (Failure _) -> Left "Parsing failed"
    (Success cs) ->
      let overlaps = trackOverlap . concat $ fmap claim2Points cs
          overlappingInches = Map.size $ Map.filter (>= 2) overlaps
       in case notOverlapping cs of
            Nothing -> Left "You deaded"
            Just x -> Right . Text.pack . show $ Day2Result x overlappingInches
  where
    claims = traverse (parseString claimP mempty . Text.unpack) $ Text.lines t

prog :: DayProg
prog = DayProg "day2" run

claimOverlap :: Claim -> Claim -> Maybe Overlap
claimOverlap c1 c2 = do
  y <- yOverlap c1 c2
  x <- xOverlap c1 c2
  let overlap = x * y
  if overlap > 0
    then Just . Overlap $ overlap
    else Nothing
  where
    yOverlap Claim {top = t1, height = h1} Claim {top = t2, height = h2} =
      let lowerBound = min (t1 + h1) (t2 + h2)
        -- ^ The higher edge of the two bottom square edges
          upperBound = max t1 t2
        -- ^ The lower edge of the two top square edges
          oy = lowerBound - upperBound
       in if oy > 0
            then Just oy
            else Nothing
    xOverlap Claim {left = l1, width = w1} Claim {left = l2, width = w2} =
      let rightBound = min (l1 + w1) (l2 + w2)
          leftBound = max l1 l2
          oy = rightBound - leftBound
       in if oy > 0
            then Just oy
            else Nothing
