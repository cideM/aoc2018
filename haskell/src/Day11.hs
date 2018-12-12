{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day11 where

import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Semigroup as Semigroup
import qualified Data.Text as Text
import Data.Text (Text)
import Debug.Trace
import qualified Data.Vector as Vector
import Data.Vector (Vector, (!?))
import Types

type Cell = (Int, Int, Int) -- ^ x y powerlevel

type Grid = Vector (Vector Cell)

powerlevel :: Int -> (Int, Int) -> Int
powerlevel serialNum (x, y) =
    let rackID = x + 10
     in (((rackID * y + serialNum) * rackID) `div` 100 `mod` 10) - 5

makeGrid :: Int -> Int -> Grid
makeGrid serialNum size =
    Vector.generate
        size
        (\y' ->
             Vector.generate
                 size
                 (\x' ->
                      let x = x' + 1 -- ^ See gridSize comment
                          y = y' + 1
                          p = powerlevel serialNum (x, y)
                       in (x, y, p)))

getCell :: Vector (Vector Cell) -> Int -> Int -> Maybe Cell
getCell grid x y = grid !? (y - 1) >>= flip (!?) (x - 1)

getMissingCells :: Grid -> Cell -> Int -> Maybe [Cell]
getMissingCells grid (x, y, _) squareSize =
    sequence $ missingCellsRight ++ missingCellsBottom
  where
    getCell' = getCell grid
    missingCellsRight =
        [ getCell' (x + squareSize) y'
            -- | Skip last one otherwise we're counting it twice
        | y' <- [y .. y + (squareSize - 1)]
        ]
    missingCellsBottom =
        [getCell' x' (y + squareSize) | x' <- [x .. x + squareSize]]

type MaxFuel = Int

type BestSquare = Int

-- TODO: comments
getBestSquare :: Grid -> Int -> (Maybe Cell, BestSquare, MaxFuel, Map Cell Int)
getBestSquare grid maxSquareSize =
    Foldable.foldr'
        (\currentSquareSize cache ->
             traceShow currentSquareSize $ Foldable.foldr' (sumRow currentSquareSize) cache grid')
        (Nothing, 0, 0, Map.empty)
        (reverse [0 .. maxSquareSize])
  where
    grid' = Vector.foldr1' (Vector.++) grid -- ^ grid flattened
    getMissingCells' = getMissingCells grid
    sumRow sqSize cell@(_, _, power) (origin, bestSqSize, bestFuel, cache) =
        let missingFuel =
                maybe
                    0
                    (Foldable.foldMap (\(_, _, plevel) -> Semigroup.Sum plevel))
                    (getMissingCells' cell sqSize)
            cachedFuel = Map.lookup cell cache
         in maybe
                (Just cell, sqSize, power, Map.insert cell power cache)
                (\cached ->
                     let fuel = Semigroup.getSum missingFuel + cached
                      in if fuel > bestFuel
                             then ( Just cell
                                  , sqSize
                                  , fuel
                                  , Map.insert cell fuel cache)
                             else ( origin
                                  , bestSqSize
                                  , bestFuel
                                  , Map.insert cell fuel cache))
                cachedFuel

gridSize :: Int
gridSize = 299 -- ^ Vector.generate starts from 0 so we just add 1 but then need to stop at 299

-- | Square size 3 means current cell + 2 so when we return we need to add 1 to
-- reflect the dimensions of the square rather than its extent from the origin
run :: Text -> Either ErrMsg Text
run t = Right . Text.pack . show $ (cell, squareSize + 1)
  where
    (cell, squareSize, _, _) = getBestSquare grid 300
    grid = makeGrid serialNum gridSize
    serialNum = read $ Text.unpack t

prog :: DayProg
prog = DayProg "day9" run
