{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Day15 where

import qualified Control.Monad as Monad
import qualified Data.Array.Unboxed as UArray
import Data.Array.Unboxed (UArray, (!))
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.Extra as Extra
import qualified Data.Map.Strict as Map'
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Debug.Trace
import Text.RawString.QQ
import Types

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
  { hp :: !Int
  , dmg :: !Int
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

otherTeam :: GameState -> Coords -> Team
otherTeam state coords =
  if team ((Map'.!) state coords) == Goblin
    then Elf
    else Goblin

hasTargetInReach :: GameState -> Coords -> Bool
hasTargetInReach state coords =
  any (maybe False ((==) targetTeam . team) . (`Map'.lookup` state)) $
  surroundingTiles coords
  where
    targetTeam = otherTeam state coords

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
             else traceShow rounds (go lastState (rounds + 1))

getHPLeft :: GameState -> Int
getHPLeft = Foldable.foldl (\acc Unit {..} -> hp + acc) 0 . Map'.elems

run :: Text -> Either ErrMsg Text
run t =
  let grid = parseInput t
      stateP1 = getUnits (Unit 200 3) grid
      p1 =
        let (rounds, lastState) = run' 100 grid stateP1
            hpLeft = getHPLeft lastState
        in "Part1: " ++
           "Rounds: " ++
           show rounds ++
           " Hp left: " ++ show hpLeft ++ " Result: " ++ show (rounds * hpLeft)
      p2 =
        let getElves =
              Map'.size .
              Map'.filter ((> 0) . hp) . Map'.filter ((== Elf) . team)
            states =
              [ ( getUnits
                    (\team ->
                       Unit
                         200
                         (if team == Goblin
                            then 3
                            else p)
                         team)
                    grid
                , p)
              | p <- [4 .. 50]
              ]
            results =
              map
                (\(state, p) ->
                   let elves = getElves state
                       (rounds, lastState) = run' 100 grid state
                       elvesFinal = getElves lastState
                       hpLeft = getHPLeft lastState
                   in (p, elves - elvesFinal, rounds * hpLeft))
                states
            best = List.find (\(_, deadElves, _) -> deadElves == 0) results
        in "Part2: " ++ show best
  in Right . Text.pack $ show p1 ++ "\n" ++ show p2

prog :: DayProg
prog = DayProg "day11" run

-- Debugging stuff
alignGrid :: Battlefield -> [((Int, Int), Char)]
alignGrid field =
  concat .
  List.groupBy (\a b -> snd (fst a) == snd (fst b)) .
  List.sortBy (\a b -> snd (fst a) `compare` snd (fst b)) $
  UArray.assocs field

debugThisShitIHateHaskell :: Battlefield -> GameState -> IO ()
debugThisShitIHateHaskell f s =
  let gridSize = uncurry max . snd $ UArray.bounds f
      printUnit defaultValue coords =
        maybe
          defaultValue
          (\Unit {..} ->
             if team == Goblin
               then 'G'
               else 'E') $
        Map'.lookup coords s
      withUnits =
        map (\((x, y), char) -> printUnit char (Coords x y)) gridWithoutUnits
      gridWithoutUnits =
        map
          (\(i, char) ->
             if char `elem` ("EG" :: String)
               then (i, '.')
               else (i, char))
          grouped
      grouped = alignGrid f
      state' =
        Text.unlines .
        map (Text.pack . \(k, v) -> show k ++ " " ++ show v) .
        List.sortBy (\(Coords _ y, _) (Coords _ y', _) -> y `compare` y') $
        Map'.assocs s
  in TextIO.putStrLn state' >>
     (TextIO.putStrLn . Text.unlines . map (Text.unwords . map Text.singleton) $
      Extra.chunksOf (gridSize + 1) withUnits)

runDebug :: Int -> Battlefield -> GameState -> IO ()
runDebug maxRounds battlefield state = go state 0
  where
    go state' rounds
      | isGameOver state' = TextIO.putStrLn . Text.pack $ show (rounds, state')
      | rounds == maxRounds =
        TextIO.putStrLn . Text.pack $ show (rounds, state')
      | otherwise =
        let (lastState, unitsLeft) = tick battlefield state'
        in debugThisShitIHateHaskell battlefield lastState >>
           if unitsLeft > 0
             then TextIO.putStrLn . Text.pack $ show (rounds, lastState)
             else go lastState (rounds + 1)

-- Elf should move right since *targets* are sorted in reading order
edgeCase1 :: Text
edgeCase1 =
  Text.strip
    [r|
#######
#######
#.E..G#
#.#####
#G#####
#######
#######
        |]

-- Elf should move left
edgeCase2 :: Text
edgeCase2 =
  Text.strip
    [r|
########
#..E..G#
#G######
########
        |]

testData2 :: Text
testData2 =
  Text.strip
    [r|
#######
#G..#E#
#E#E.E#
#G.##.#
#...#E#
#...E.#
#######
    |]

edgeCase3 :: Text
edgeCase3 =
  Text.strip
    [r|
#####
#.E.#
#.G.#
#.G.#
#...#
#####
        |]

testDataFuckYou :: Text
testDataFuckYou =
  Text.strip
    [r|
#########
#G..G..G#
#.......#
#.......#
#G..E..G#
#.......#
#.......#
#G..G..G#
#########
        |]
