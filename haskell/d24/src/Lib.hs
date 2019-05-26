{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Lib where

import           Control.Monad              (forM_, when)
import           Control.Monad.Loops        (whileM)
import           Control.Monad.State.Lazy   (MonadState)
import qualified Control.Monad.State.Lazy   as S
import qualified Data.Foldable              as F
import           Data.Functor               (($>))
import           Data.List                  (find, maximumBy, sortBy, sortOn)
import qualified Data.List                  as List
import qualified Data.List.Extra            as Extra
import qualified Data.Map.Strict            as M'
import           Data.Ord                   (Down (..), comparing)
import qualified Data.Sequence              as Seq
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           Debug.Pretty.Simple
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Pretty.Simple         (pPrint)
import           Types

getSelectOrder :: GameState -> [(GroupID, Group Int)]
getSelectOrder = sortOn (Down . snd) . M'.assocs

findBestTarget :: [((GroupID, Group Int), Int)] -> ((GroupID, Group Int), Int)
findBestTarget = List.maximumBy bestTarget
  where
    bestTarget ((_, group1), damageReceived1) ((_, group2), damageReceived2) =
      mconcat
        [ damageReceived1 `compare` damageReceived2
        , comparing getEP group1 group2
        , comparing _initiative group1 group2
        ]

getTargetsWithDamageReceived ::
     Group Int -> Set.Set (GroupID, Group Int) -> [((GroupID, Group Int), Int)]
getTargetsWithDamageReceived attacker defenders =
  let defenders' =
        Set.toList . Set.filter (\(_, d) -> _team d /= _team attacker) $
        defenders
   in filter (\(_, damage) -> damage > 0) $
      map (\x@(id, defender) -> ((id, defender), getDamage attacker defender)) defenders'

-- | The function only reads from GameState. I should eventually address this
-- glaring type safety hole
assignTargets :: (MonadState GameState m) => m (M'.Map GroupID GroupID) -- Attacker, Defender
assignTargets = do
  s <- S.get
  let assocs = M'.assocs s
      selectionOrder = getSelectOrder s
  return $ go selectionOrder (Set.fromList assocs) M'.empty
  where
    go [] _ out = out
    go ((attackerID, attacker):as) defenders out
      | Set.null defenders = out
      | otherwise =
        let targets = getTargetsWithDamageReceived attacker defenders
         in if null targets
              then go as defenders out
              else let (target@(targetID, _), damageReceived) =
                         findBestTarget targets
                    in go as (Set.delete target defenders) $
                       M'.insert attackerID targetID out

dealDamage :: (MonadState GameState m) => GroupID -> Int -> m ()
dealDamage id damage = S.modify $ M'.update f id
  where
    f target =
      let deadUnits = damage `quot` _hitPointsPerUnit target
          remainingUnits = _unitCount target - deadUnits
       in if remainingUnits <= 0
            then Nothing
            else Just $ target {_unitCount = remainingUnits}

fight :: (MonadState GameState m) => m ()
fight = do
  ts <- assignTargets
  s <- S.get
  let sorted = fmap fst . sortOn (Down . _initiative . snd) $ M'.assocs s
  forM_
    sorted
    (\attackerID ->
       S.get >>= \s ->
         when (M'.member attackerID s) $
         case M'.lookup attackerID ts of
           Nothing -> pure ()
           Just defenderID ->
             when (M'.member defenderID s) $
             dealDamage defenderID $
             getDamage (s M'.! attackerID) (s M'.! defenderID))

getDamage ::
     (Num a)
  => Group a
  -- ^ Attacker
  -> Group a
  -- ^ Defender
  -> a
getDamage g1 g2 =
  let attrs = _attributes g2
      multiplier
        | ImmuneTo (_damageType g1) `elem` attrs = 0
        | WeakTo (_damageType g1) `elem` attrs = 2
        | otherwise = 1
   in getEP g1 * multiplier

hasWinner :: (MonadState GameState m) => m Bool
hasWinner = do
  s <- S.get
  let (a:b) = List.groupBy (\a b -> _team a == _team b) $ M'.elems s
  return (null a || null b)
