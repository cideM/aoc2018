{-# LANGUAGE OverloadedStrings #-}

module TestData where

import qualified Data.Map.Strict as M'
import qualified Data.Text       as T
import           Types

-- | Test data taken from the actual input
is1 :: Group Int
is1 = Group 2743 4149 14 13 Radiation [] $ TeamName "Immune System"

is2 :: Group Int
is2 = Group 8829 7036 15 7 Fire [] $ TeamName "Immune System"

is3 :: Group Int
is3 =
  Group
    1928
    10700
    3
    50
    Slashing
    [WeakTo Cold, ImmuneTo Fire, ImmuneTo Radiation, ImmuneTo Slashing] $
  TeamName "Immune System"

is4 :: Group Int
is4 = Group 6051 11416 20 15 Bludgeoning [] $ TeamName "Immune System"

is5 :: Group Int
is5 =
  Group 895 10235 10 92 Bludgeoning [ImmuneTo Slashing, WeakTo Bludgeoning] $
  TeamName "Immune System"

is6 :: Group Int
is6 = Group 333 1350 12 36 Radiation [] $ TeamName "Immune System"

is7 :: Group Int
is7 = Group 2138 8834 11 35 Cold [WeakTo Bludgeoning] $ TeamName "Immune System"

is8 :: Group Int
is8 =
  Group 4325 1648 8 3 Bludgeoning [WeakTo Cold, WeakTo Fire] $
  TeamName "Immune System"

is9 :: Group Int
is9 =
  Group 37 4133 1 1055 Radiation [ImmuneTo Radiation, ImmuneTo Slashing] $
  TeamName "Immune System"

is10 :: Group Int
is10 =
  Group 106 3258 13 299 Cold [ImmuneTo Slashing, ImmuneTo Radiation] $
  TeamName "Immune System"

in1 :: Group Int
in1 = Group 262 8499 6 45 Cold [WeakTo Cold] $ TeamName "Infection"

in2 :: Group Int
in2 =
  Group 732 47014 17 127 Bludgeoning [WeakTo Cold, WeakTo Bludgeoning] $
  TeamName "Infection"

in3 :: Group Int
in3 = Group 4765 64575 18 20 Radiation [] $ TeamName "Infection"

in4 :: Group Int
in4 =
  Group 3621 19547 5 9 Cold [ImmuneTo Radiation, ImmuneTo Cold] $
  TeamName "Infection"

in5 :: Group Int
in5 =
  Group
    5913
    42564
    9
    14
    Slashing
    [ImmuneTo Radiation, ImmuneTo Bludgeoning, ImmuneTo Fire] $
  TeamName "Infection"

in6 :: Group Int
in6 =
  Group
    7301
    51320
    2
    11
    Fire
    [WeakTo Radiation, WeakTo Fire, ImmuneTo Bludgeoning] $
  TeamName "Infection"

in7 :: Group Int
in7 =
  Group 3094 23713 19 14 Radiation [WeakTo Slashing, WeakTo Fire] $
  TeamName "Infection"

in8 :: Group Int
in8 =
  Group 412 36593 16 177 Slashing [WeakTo Radiation, WeakTo Bludgeoning] $
  TeamName "Infection"

in9 :: Group Int
in9 = Group 477 35404 7 146 Cold [] $ TeamName "Infection"

in10 :: Group Int
in10 = Group 332 11780 4 70 Slashing [WeakTo Fire] $ TeamName "Infection"

addIndex =
  map
    (\(i, x) ->
       let (TeamName name) = _team x
        in (GroupID $ T.pack (show i) <> "-" <> name, x)) .
  zip [1 ..]

makeState immune infection =
  let a = M'.fromList $ addIndex immune
      b = M'.fromList $ addIndex infection
   in M'.union a b

state :: GameState
state =
  makeState
    [in1, in2, in3, in4, in5, in6, in7, in8, in9, in10]
    [is1, is2, is3, is4, is5, is6, is7, is8, is9, is10]
