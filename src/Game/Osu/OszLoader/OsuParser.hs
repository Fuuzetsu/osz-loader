{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Osu.OszLoader.OsuParser where

import Control.Applicative
import Data.Text
import Data.Attoparsec.Text
import Game.Osu.OszLoader.Types
import Game.Osu.OszLoader.OsuParser.General

-- $setup
-- >>> import Control.Applicative
-- >>> import Data.Attoparsec.Text
-- >>> import Data.Text
-- >>> import Test.QuickCheck
-- >>> :set -XOverloadedStrings
-- >>> :set -XUnicodeSyntax


parseOsu ∷ Text → Either String OsuMap
parseOsu = parseOnly osuParser

osuParser ∷ Parser OsuMap
osuParser = do
  fv ← fileFormat
  skipSpace
  gen ← generalSection
  return $ OsuMap { _formatVersion = fv
                  , _general = gen
                  , _editor = undefined
                  , _metadata = undefined
                  , _difficulty = undefined
                  , _timingPoints = undefined
                  , _colours = undefined
                  , _hitObjects = undefined
                  }

{-|
Parses file format version. We currently don't change parsing
behaviour based on this but it's there for information.

>>> parseOnly fileFormat "osu file format v7"
Right 7
-}
fileFormat ∷ Parser Int
fileFormat = "osu file format v" *> decimal
