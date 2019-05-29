#!/usr/bin/env stack
{-
    stack
    script
    --resolver lts-13.21
    --package text,megaparsec,containers,mtl,extra,monad-loops,pretty-simple
-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

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

{-
#########################
        Types
#########################
-}
newtype TeamName =
  TeamName Text
  deriving (Show, Eq)

newtype GroupID =
  GroupID Text
  deriving (Show, Ord, Eq)

data DamageType
  = Fire
  | Cold
  | Radiation
  | Slashing
  | Bludgeoning
  deriving (Eq, Show)

data Attribute
  = WeakTo DamageType
  | ImmuneTo DamageType
  deriving (Eq, Show)

data Group a = Group
  { _unitCount        :: a
  , _hitPointsPerUnit :: a
  , _initiative       :: a
  , _damage           :: a
  , _damageType       :: DamageType
  , _attributes       :: [Attribute]
  , _team             :: TeamName
  } deriving (Eq, Show)

-- Decreasing order of effective power and in case of tie higher
-- initiative
instance (Ord a, Num a) => Ord (Group a) where
  g1 `compare` g2 =
    mconcat
      [comparing (Down . getEP) g1 g2, comparing (Down . _initiative) g1 g2]

type Groups a = M'.Map GroupID (Group a)

type GameState = Groups Int

{-
#########################
        Logic
#########################
-}
getEP :: (Num a) => Group -> a
getEP g = _unitCount g * _damage g

-- | The function only reads from GameState. I should eventually address this
-- glaring type safety hole
assignTargets :: (MonadState GameState m) => m (M'.Map GroupID GroupID) -- Attacker, Defender
assignTargets = do
  s <- S.get
  let assocs = M'.assocs s
      selectionOrder = sortOn snd assocs
  return $ go selectionOrder (Set.fromList assocs) M'.empty
  where
    bestTarget ((_, group1), damageReceived1) ((_, group2), damageReceived2) =
      mconcat
        [ damageReceived1 `compare` damageReceived2
        , comparing getEP group1 group2
        , comparing _initiative group1 group2
        ]
    go [] _ out = out
    go ((attackerID, attacker):as) defenders out
      | Set.null defenders = out
      | otherwise =
        let defenders' =
              Set.toList . Set.filter (\(_, d) -> _team d /= _team attacker) $
              defenders
            targets =
              zip defenders' . filter (> 0) $
              map (\(_, defender) -> getDamage attacker defender) defenders'
         in if null targets
              then go as defenders out
              else let (target@(targetID, _), damageReceived) =
                         List.maximumBy bestTarget targets
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

main :: IO ()
main = do
  parsed <- runParser inputParser "stdin" . T.pack <$> getContents
  case parsed of
    Left e -> pPrint e
    Right s -> do
      let s' = S.execState (whileM (not <$> hasWinner) fight) $ M'.unions s
      pPrint s'
      pPrint . sum . map _unitCount $ M'.elems s'

{-
#########################
        Parsing
#########################
-}
type Parser = Parsec Void Text

damageTypeParser :: Parser DamageType
damageTypeParser =
  space *>
  ((string "fire" $> Fire) <|> (string "radiation" $> Radiation) <|>
   (string "bludgeoning" $> Bludgeoning) <|>
   (string "cold" $> Cold) <|>
   (string "slashing" $> Slashing)) <*
  space

attributesParser :: Parser [Attribute]
attributesParser = do
  _ <- space
  attributeTypeConstructor <-
    (string "weak" $> WeakTo) <|> (string "immune" $> ImmuneTo)
  _ <- string " to" <* space
  damageTypes <- damageTypeParser `sepBy` char ',' <* space
  return $ fmap attributeTypeConstructor damageTypes

attributesParser' :: Parser [Attribute]
attributesParser' = concat <$> attributesParser `sepBy` char ';'

parens = between (char '(') (char ')')

groupParser :: TeamName -> Parser (Group Int)
groupParser tn = do
  unitCount <- L.decimal <* space
  _ <- string "units each with "
  hitPointsPerUnit <- L.decimal
  _ <- string " hit points" <* space
  attributes <- option [] (parens attributesParser' <* space)
  attack <- string "with an attack that does " *> L.decimal <* space
  damageType <- damageTypeParser <* "damage at initiative" <* space
  initiative <- L.decimal <* space
  return $
    Group
      { _unitCount = unitCount
      , _hitPointsPerUnit = hitPointsPerUnit
      , _attributes = attributes
      , _damage = attack
      , _damageType = damageType
      , _initiative = initiative
      , _team = tn
      }

groupsParser :: Parser (Groups Int)
groupsParser = do
  name <-
    T.pack <$> (manyTill (choice [letterChar, spaceChar]) (char ':') <* space)
  groups <- many (groupParser $ TeamName name)
  let ids = makeId name <$> [0 ..]
  return . M'.fromList $ zip ids groups
  where
    makeId prefix = GroupID . (<>) (prefix <> "-") . T.pack . show

inputParser = many groupsParser <* space
