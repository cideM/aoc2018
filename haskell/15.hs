#!/usr/bin/env stack
{-
    stack
    script
    --resolver lts-12.20
    --package text,trifecta,containers,array,extra
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Day15 where

import qualified Control.Monad      as Monad
import           Data.Array.Unboxed (UArray, (!))
import qualified Data.Array.Unboxed as UArray
import qualified Data.Foldable      as Foldable
import qualified Data.List          as List
import qualified Data.List.Extra    as Extra
import qualified Data.Map.Strict    as Map'
import qualified Data.Maybe         as Maybe
import qualified Data.Set           as Set
import           Data.Text          (Text)
import qualified Data.Text          as Text

-- It works. That sums up the positive things about this module. ~_~
-- TODO: Maybe refactor this some day
data Coords =
  Coords Int
         Int
  deriving (Eq, Show)

instance Ord Coords where
  (Coords x y) `compare` (Coords x' y') =
    if y /= y'
      then y `compare` y'
      else x `compare` x'

data Team
  = Goblin
  | Elf
  deriving (Eq, Show)

data Unit = Unit
  { hp   :: !Int
  , dmg  :: !Int
  , team :: Team
  } deriving (Show)

type Battlefield = UArray (Int, Int) Char

type GameState = Map'.Map Coords Unit

parseInput :: Text -> Battlefield
parseInput input =
  UArray.array
    ((0, 0), (maxX, maxY))
    [((x, y), char) | (y, row) <- indexedLines, (x, char) <- zip [0 ..] row]
  where
    str = Text.unpack input
    indexedLines = zip [0 ..] $ Prelude.lines str
    maxX = flip (-) 1 $ Prelude.length . snd $ head indexedLines
    maxY = flip (-) 1 $ Prelude.length indexedLines

getUnits :: (Team -> Unit) -> Battlefield -> GameState
getUnits makeUnit battlefield = foldl f Map'.empty $ UArray.assocs battlefield
  where
    f acc ((x, y), tile)
      | tile == 'G' = Map'.insert (Coords x y) (makeUnit Goblin) acc
      | tile == 'E' = Map'.insert (Coords x y) (makeUnit Elf) acc
      | otherwise = acc

isGameOver :: GameState -> Bool
isGameOver = (==) 1 . List.length . List.nub . map team . Map'.elems

getTargets :: Coords -> GameState -> Maybe [(Coords, Unit)]
getTargets coords state = do
  Unit {team = currentTeam} <- Map'.lookup coords state
  let targetTeam =
        if currentTeam == Goblin
          then Elf
          else Goblin
  return $ filter ((==) targetTeam . team . snd) $ Map'.assocs state

surroundingTiles :: Coords -> [Coords]
surroundingTiles (Coords x y) =
  Extra.sort $
  map (uncurry Coords) [(x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)]

inGrid :: Battlefield -> Coords -> Bool
inGrid grid (Coords x y) =
  let ((minX, minY), (maxX, maxY)) = UArray.bounds grid
   in x >= minX && x <= maxX && y >= minY && y <= maxY

tileIsFree :: GameState -> Battlefield -> Coords -> Bool
tileIsFree state battlefield target =
  let lookup' key = Map'.lookup key state
      isNotAWall (Coords x' y') = (/=) '#' $ battlefield ! (x', y')
   in Maybe.isNothing (lookup' target) && isNotAWall target

pathToTarget :: Battlefield -> GameState -> Coords -> Maybe Coords
pathToTarget battlefield state start = do
  targets <- getTargets start state
  let targetCells = concatMap (surroundingTiles . fst) targets
  Monad.guard . not $ start `elem` targetCells
  let initial =
        [ (tile, tile)
        | tile <- surroundingTiles start
        , all ($ tile) [isInGridAndNotWall, isFree]
        ]
  go targetCells initial $ Set.singleton start
  where
    isInGridAndNotWall c@(Coords x y) =
      let char = battlefield ! (x, y)
          isNotAWall = char /= '#'
          isInGrid = inGrid battlefield c
       in isInGrid && isNotAWall
    isFree = tileIsFree state battlefield
    go _ [] _ = Nothing
    go targetCells xs seen =
      let isUnknown c = not $ Set.member c seen
          targetsInReach = filter (flip elem targetCells . fst) xs
       in if not $ null targetsInReach
            then Just . snd $
                 List.minimumBy (\a b -> fst a `compare` fst b) targetsInReach
            else let newFrontiers =
                       List.nubBy (\a b -> fst a == fst b) $
                       concatMap
                         (\(current, first) ->
                            [ (tile, first)
                            | tile <- surroundingTiles current
                            , all
                                ($ tile)
                                [isUnknown, isFree, isInGridAndNotWall]
                            ])
                         xs
                  in go
                       targetCells
                       newFrontiers
                       (Set.union seen (Set.fromList (map fst newFrontiers)))

move :: Battlefield -> GameState -> Coords -> Maybe (Coords, GameState)
move battlefield state coords = do
  unit <- Map'.lookup coords state
  nextStep <- pathToTarget battlefield state coords
  return (nextStep, Map'.insert nextStep unit (Map'.delete coords state))

targetsForUnit :: GameState -> Coords -> Maybe [(Coords, Unit)]
targetsForUnit state coords = do
  unit <- Map'.lookup coords state
  let potentialTargets =
        Maybe.mapMaybe (\coords' -> (,) coords' <$> Map'.lookup coords' state) $
        surroundingTiles coords
  Monad.guard $ not (null potentialTargets)
  return $ filter (\(_, target) -> team target /= team unit) potentialTargets

attack :: GameState -> Coords -> Maybe GameState
attack state coords = do
  targets <- targetsForUnit state coords
  Unit {dmg = attackerDmg} <- Map'.lookup coords state
  Monad.guard $ not (null targets)
  return $
    let (targetCoords, _) =
          List.minimumBy
            (\(_, unit1) (_, unit2) -> hp unit1 `compare` hp unit2)
            targets
     in Map'.filter (\Unit {..} -> hp > 0) $
        Map'.adjust
          (\target@Unit {..} -> target {hp = hp - attackerDmg})
          targetCoords
          state

tick :: Battlefield -> GameState -> (GameState, Int)
tick battlefield state =
  let readingOrder =
        List.sortBy (\(coords, _) (coords', _) -> coords `compare` coords') $
        Map'.assocs state
   in foldl
        (\(acc, unitsLeft) (coords, Unit {..}) ->
           if isGameOver acc
             then (acc, unitsLeft)
             else let (newCoords, newState) =
                        Maybe.fromMaybe (coords, acc) $
                        move battlefield acc coords
                   in ( Maybe.fromMaybe newState $ attack newState newCoords
                      , unitsLeft - 1))
        (state, Map'.size state)
        readingOrder

run' :: Int -> Battlefield -> GameState -> (Int, GameState)
run' maxRounds battlefield state = go state 0
  where
    go state' rounds
      | isGameOver state' = (rounds, state')
      | rounds == maxRounds = (rounds, state')
      | otherwise =
        let (lastState, unitsLeft) = tick battlefield state'
         in if unitsLeft > 0
              then (rounds, lastState)
              else go lastState (rounds + 1)

getHPLeft :: GameState -> Int
getHPLeft = Foldable.foldl (\acc Unit {..} -> hp + acc) 0 . Map'.elems

getElves :: GameState -> Int
getElves = Map'.size . Map'.filter ((> 0) . hp) . Map'.filter ((== Elf) . team)

main :: IO ()
main = do
  grid <- parseInput . Text.pack <$> getContents
  let stateP1 = getUnits (Unit 200 3) grid
      (rounds, lastState) = run' 100 grid stateP1
      hpLeft = getHPLeft lastState
  -- | Part 1
  print $
    "Rounds: " <> show rounds <> " HP left: " <> show hpLeft <> " Result: " <>
    show (rounds * hpLeft)
  -- | Part 2
  print . List.find (\(_, deadElves, _) -> deadElves == 0) $
    map (uncurry (runState grid)) (mkStates grid)
    -- | Run the different scenarios we generated with mkStates
  where
    runState :: Battlefield -> GameState -> Int -> (Int, Int, Int)
    runState grid state power =
      let (rounds', lastState') = run' 100 grid state
          hpLeft' = getHPLeft lastState'
       in (power, getElves state - getElves lastState', rounds' * hpLeft')
    -- | Helper function to generate units from a grid. No idea how I came up
    -- with this convoluted crap.
    makeUnitFunc _ Goblin = Unit 200 3 Goblin
    makeUnitFunc p _      = Unit 200 p Elf
    -- | Generate different scenarios where elf power varies
    mkStates grid = do
      p <- [4 .. 50]
      return (getUnits (makeUnitFunc p) grid, p)
