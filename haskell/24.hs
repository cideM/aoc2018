#!/usr/bin/env stack
{-
    stack
    script
    --resolver lts-13.16
    --package text
-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

type DamageType = Text

data Attribute
  = WeakTo DamageType
  | ImmuneTo DamageType

data Group a = Group
  { _name :: Text
  , _unitCount :: a
  , _hitPointsPerUnit :: a
  , _initiative :: a
  , _damage :: a
  , _effectivePower :: a,
  , _damageType :: DamageType
  , _attributes :: [Attribute]
  }

main :: IO ()
main = print "Hello"
