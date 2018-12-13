{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Day11 where

import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Semigroup as Semigroup
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Vector as Vector
import Data.Vector (Vector, (!?))
import Debug.Trace
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

-- | If we calculate this square first
-- o o #
-- o o #
-- # # #
-- and then this
-- o o o
-- o o o
-- o o o
-- we cache the first grid and then add the new cells (or rather their summed
-- fuel value) to the old cache.
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

-- | Takes a grid and the maximum square size. Iterates over the square sizes,
-- computing the square with the most fuel in all possible grids.
getBestSquare :: Grid -> Int -> (Maybe Cell, BestSquare, MaxFuel, Map Cell Int)
getBestSquare grid maxSquareSize =
    Foldable.foldr'
        (\currentSquareSize cache ->
             traceShow currentSquareSize $
             Foldable.foldr' (sumRow currentSquareSize) cache grid')
        (Nothing, 0, 0, Map.empty)
        (reverse [0 .. maxSquareSize])
  where
    grid' = Vector.foldr1' (Vector.++) grid -- ^ grid flattened
    getMissingCells' = getMissingCells grid
    -- | I cache the fuel for all cells for each square size we're going
    -- through. So when I'm calculating the squares of square size 5 I can use
    -- the cached values of square size 4 and just add the new cells to the
    -- mix.
    sumRow ::
           Int
        -> Cell
        -> (Maybe Cell, BestSquare, MaxFuel, Map Cell Int)
        -> (Maybe Cell, BestSquare, MaxFuel, Map Cell Int)
    sumRow sqSize cell@(_, _, power)
        -- | Don't break this line...
                   (origin
                    -- ^ the origin of the square that currently has the most fuel
                    , bestSqSize
                      -- ^ the square size of the above square
                      , bestFuel
                        -- ^ fuel in the above square
                        , cache)
                        -- ^ the map holding the squares and fuels from the last iteration
     =
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