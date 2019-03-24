#!/usr/bin/env stack
{-
    stack
    script
    --resolver lts-13.6
    --package transformers,mtl,megaparsec
-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser a = Parsec Void a


