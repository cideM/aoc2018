#!/usr/bin/env stack
{-
    stack
    script
    --resolver lts-13.16
    --package text,megaparsec
-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type DamageType = Text

data Attribute
  = WeakTo DamageType
  | ImmuneTo DamageType
  deriving (Eq, Show)

data Group a = Group
  { _name :: Text
  , _unitCount :: a
  , _hitPointsPerUnit :: a
  , _initiative :: a
  , _damage :: a
  , _effectivePower :: a
  , _damageType :: DamageType
  , _attributes :: [Attribute]
  } deriving (Eq, Show)

type Parser = Parsec Void Text

groupParser :: Parser (Group Int)
groupParser = do
  groupName <- many (oneOf [spaceChar, letterChar]) <?> "group name"
  _ <- symbolChar ";"
  return $ Group (T.concat $ map T.singleton groupName) 0 0 0 0 0 "Foo" []

main :: IO ()
main = print "Hello"
