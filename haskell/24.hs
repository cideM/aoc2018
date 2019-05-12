#!/usr/bin/env stack
{-
    stack
    script
    --resolver lts-13.16
    --package text,megaparsec
-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

import           Data.Functor               (($>))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void                  (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Team a = Team
  { _name   :: Text
  , _groups :: [Group a]
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
  , _effectivePower   :: a
  , _damageType       :: DamageType
  , _attributes       :: Maybe [Attribute]
  } deriving (Eq, Show)

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
  attributes <- optional $ parens attributesParser' <* space
  attack <- string "with an attack that does " *> L.decimal <* space
  damageType <- damageTypeParser <* "damage at initiative" <* space
  initiative <- L.decimal <* space
  return $
    Group
      { _unitCount = unitCount
      , _hitPointsPerUnit = hitPointsPerUnit
      , _attributes = attributes
      , _damage = attack
      , _effectivePower = unitCount * attack
      , _damageType = damageType
      , _initiative = initiative
      }

teamParser :: Parser (Team Int)
teamParser = do
  name <-
    T.pack <$> (manyTill (choice [letterChar, spaceChar]) (char ':') <* space)
  groups <- many groupParser
  return $ Team name groups

inputParser = do
  many teamParser <* space

main :: IO ()
main = do
  parsed <- runParser inputParser "stdin" . T.pack <$> getContents
  print parsed
