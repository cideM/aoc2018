{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Day15 where

-- FIXME: This program is excruciatingly slow. No idea why. Profiling revealed
-- that `go` is slow, big surprise. -_-

import qualified Control.Monad as Monad
import qualified Data.Array.Unboxed as UArray
import Data.Array.Unboxed (UArray, (!))
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.Extra as Extra
import qualified Data.Map.Strict as Map'
import qualified Data.Maybe as Maybe
import qualified Data.Sequence as Seq
import Data.Sequence (Seq((:<|), Empty), (><))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Debug.Trace
import Text.RawString.QQ
import Types

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

makeUnit :: Int -> Team -> Unit
makeUnit power team =
  Unit
    200
    (if team == Elf
       then power
       else 3)
    team

getUnits :: Int -> Battlefield -> GameState
getUnits power battlefield = foldl f Map'.empty $ UArray.assocs battlefield
  where
    f acc ((x, y), tile)
      | tile == 'G' = Map'.insert (Coords x y) (makeUnit power Goblin) acc
      | tile == 'E' = Map'.insert (Coords x y) (makeUnit power Elf) acc
      | otherwise = acc

inRange :: Coords -> Coords -> Bool
inRange (Coords x y) (Coords x' y') =
  (x == x' || y == y') && (x' - x == 1 || y' - y == 1)

isGameOver :: GameState -> Bool
isGameOver = (==) 1 . List.length . List.nub . map team . Map'.elems

getTargets :: Unit -> GameState -> [(Coords, Unit)]
getTargets Unit {team = currentTeam} state =
  let targetTeam =
        if currentTeam == Goblin
          then Elf
          else Goblin
  in filter ((==) targetTeam . team . snd) $ Map'.assocs state

surroundingTiles :: Coords -> [Coords]
surroundingTiles (Coords x y) =
  map (uncurry Coords) [(x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)]

inGrid :: Battlefield -> Coords -> Bool
inGrid grid (Coords x y) =
  let ((minX, minY), (maxX, maxY)) = UArray.bounds grid
  in x >= minX && x <= maxX && y >= minY && y <= maxY

tileIsFree :: GameState -> Battlefield -> Coords -> Bool
tileIsFree state battlefield target =
  let lookup' key = Map'.lookup key state
      isNotAWall (Coords x' y') = (/=) '#' $ battlefield ! (x', y')
      isInGrid = inGrid battlefield
  in Maybe.isNothing (lookup' target) && isInGrid target && isNotAWall target

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

pathToTarget :: Battlefield -> GameState -> Coords -> Maybe [Coords]
pathToTarget battlefield state start
  | hasTargetInReach state start = Nothing
  | otherwise = go (Seq.singleton [start]) Set.empty
  where
    targetTeam = otherTeam state start
    isFree = tileIsFree state battlefield
    go Empty _ = Nothing
    go paths seen =
      let notYetSeen value = not $ Set.member value seen
          newPaths =
            concatMap
              (\path ->
                 let nextSteps =
                       filter notYetSeen . filter isFree . surroundingTiles $
                       last path
                 in map (\step -> path ++ [step]) nextSteps)
              paths
          pathsWithTargets =
            filter
              (\p ->
                 let lookups =
                       map (`Map'.lookup` state) (surroundingTiles $ last p)
                 in any (maybe False ((==) targetTeam . team)) lookups)
              newPaths
      in if not (null pathsWithTargets)
           then Just . List.minimumBy (\a b -> last a `compare` last b) $
                pathsWithTargets
           else go
                  (Seq.fromList newPaths)
                  (Set.union seen (Set.fromList (concat newPaths)))

move :: Battlefield -> GameState -> Coords -> Maybe (Coords, GameState)
move battlefield state coords = do
  unit <- Map'.lookup coords state
  (_:steps) <- pathToTarget battlefield state coords
  Monad.guard $ not (null steps)
  return
    ( Prelude.head steps
    , Map'.insert (Prelude.head steps) unit (Map'.delete coords state))

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
  Monad.guard $ not (null targets)
  return $
    let (targetCoords, _) =
          List.minimumBy
            (\(_, unit1) (_, unit2) -> hp unit1 `compare` hp unit2)
            targets
    in Map'.filter (\Unit {..} -> hp > 0) $
       Map'.adjust
         (\target@Unit {..} -> target {hp = hp - dmg})
         targetCoords
         state

tick :: Battlefield -> GameState -> (GameState, Int)
tick battlefield state =
  let readingOrder =
        List.sortBy (\(Coords _ y, _) (Coords _ y', _) -> y `compare` y') $
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

run :: Text -> Either ErrMsg Text
run t =
  let grid = parseInput t
      stateP1 = getUnits 3 grid
      stateP2 = getUnits 25 grid
      p1 =
        let (rounds, lastState) = run' 85 grid stateP1
            hpLeft =
              Foldable.foldl (\acc Unit {..} -> hp + acc) 0 $
              Map'.elems lastState
        in "rounds: " ++
           show rounds ++
           "hp left: " ++ show hpLeft ++ " result: " ++ show (rounds * hpLeft)
      p2 =
        let elves = Map'.size $ Map'.filter ((==) Elf . team) stateP2
            final = run' 30 grid stateP2
            elvesFinal = Map'.size $ Map'.filter ((==) Elf . team) $ snd final
        in "Elves died: " ++ show (elves - elvesFinal)
  in Right . Text.pack $ show p1 ++ show p2

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

prog :: DayProg
prog = DayProg "day11" run
