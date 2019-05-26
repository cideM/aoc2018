{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Data.Functor               (($>))
import qualified Data.Map.Strict            as M'
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           Text.Megaparsec            hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Types

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
  let ids = makeId name <$> [1 ..]
  return . M'.fromList $ zip ids groups
  where
    makeId prefix = GroupID . (<>) (prefix <> "-") . T.pack . show

inputParser = many groupsParser <* space
