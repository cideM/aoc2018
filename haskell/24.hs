#!/usr/bin/env stack
{-
    stack
    script
    --resolver lts-13.16
    --package text,megaparsec,containers
-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Functor               (($>))
import           Data.List                  (maximumBy, sortBy, sortOn)
import qualified Data.Map.Strict            as M'
import           Data.Ord                   (Down (..))
import qualified Data.Set                   as S
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           Debug.Trace
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Passing groups in various data structures to functions risks referring to
-- stale data. I could potentially alter a groups hit points in one function
-- but forget to merge it into the old group data that another function is
-- using (e.g., when passing groups to a function and then continuing to work
-- on the team from which I took those groups but without merging in the new
-- group data).
--
type TeamName = Text

type GroupID = Text

data Team a = Team
  { _name   :: TeamName
  , _groups :: [(GroupID, Group a)]
  } deriving (Show)

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
  } deriving (Eq, Show)

getEP :: (Num a) => Group a -> a
getEP g = _unitCount g * _damage g

hasGroup :: Team a -> GroupID -> Bool
hasGroup t id = id `elem` fmap fst $ _groups t

-- Decreasing order of effective power and in case of tie higher
-- initiative
instance (Ord a, Num a) => Ord (Group a) where
  g1 `compare` g2 =
    let ep1 = getEP g1
        ep2 = getEP g2
     in if ep1 == ep2
          then _initiative g1 `compare` _initiative g2
          else ep1 `compare` ep2

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

groupParser :: Parser (Group Int)
groupParser = do
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
      }

teamParser :: Parser (Team Int)
teamParser = do
  name <-
    T.pack <$> (manyTill (choice [letterChar, spaceChar]) (char ':') <* space)
  groups <- many groupParser
  let ids = makeId name <$> [0 ..]
  return . Team name $ zip ids groups
  where
    makeId prefix = (<>) (prefix <> "-") . T.pack . show

getTargetSelectionOrder :: (Ord a, Num a) => [(GroupID, Group a)] -> [GroupID]
getTargetSelectionOrder = fmap fst . sortOn (Down . snd)

assignTargets :: [GroupID] -> Team a -> Team a -> [(GroupID, GroupID)]
assignTargets selectionOrder t1 t2 = go selectionOrder []
  where
    go [] assoc = assoc
    go [id:ids] assoc =
      let currentGroup =
            snd . find (\(id', _) -> id == id') $ _groups t1 ++ _groups t2
          defenders = S.fromList $ map snd assoc
          targets =
            filter (\(id, _) -> not $ S.member id defenders) . _groups $
            if hasGroup t1 id
              then t2
              else t1
       in case getTarget currentGroup targets of
            Just tar -> go ids $ assoc ++ (id, tar)
            Nothing  -> ids assoc

getTarget ::
     (Show a, Ord a, Num a) => Group a -> [(GroupID, Group a)] -> Maybe GroupID
getTarget g ts
  | null ts = Nothing
  | otherwise =
    let withScore = flip zip ts $ map getTargetScore ts
     in Just . fst . snd $
        maximumBy
          (\(score1, (_, group1)) (score2, (_, group2)) ->
             if score1 /= score2
               then score1 `compare` score2
               else group1 `compare` group2)
          withScore
  where
    getTargetScore tar =
      let attrs = _attributes $ snd tar
          immunities = map (\(ImmuneTo a) -> a) $ filter immuneOnly attrs
          weaknesses = map (\(WeakTo a) -> a) $ filter weakOnly attrs
          dmgType = _damageType g
       in if dmgType `elem` immunities
            then -1
            else if dmgType `elem` weaknesses
                   then 1
                   else 0
    immuneOnly (ImmuneTo _) = True
    immuneOnly _            = False
    weakOnly (WeakTo _) = True
    weakOnly _          = False

-- fight :: (Num a) => Team a -> Team a -> Team a -> Team a
-- fight t1 t2 =
--         let order = getTargetSelectionOrder $ _groups t1 ++ _groups t2
inputParser = do
  many teamParser <* space

main :: IO ()
main = do
  parsed <- runParser inputParser "stdin" . T.pack <$> getContents
  print parsed

-- TEST DATA
g1 =
  Group
    { _unitCount = 10
    , _hitPointsPerUnit = 10
    , _initiative = 1
    , _damage = 5
    , _damageType = Fire
    , _attributes = [WeakTo Bludgeoning, ImmuneTo Cold]
    }

g2 =
  Group
    { _unitCount = 11
    , _hitPointsPerUnit = 10
    , _initiative = 1
    , _damage = 5
    , _damageType = Cold
    , _attributes = [WeakTo Fire]
    }

g3 =
  Group
    { _unitCount = 12
    , _hitPointsPerUnit = 10
    , _initiative = 1
    , _damage = 5
    , _damageType = Cold
    , _attributes = [WeakTo Radiation]
    }

t = Team {_name = "Foo", _groups = [("1", g1), ("2", g2), ("3", g3)]}
