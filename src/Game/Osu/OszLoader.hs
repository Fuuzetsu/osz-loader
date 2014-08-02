{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Osu.OszLoader where

import Data.Text
import Data.Attoparsec.Text
import Game.Osu.OszLoader.Types

parseOsu ∷ Text → Either Text OsuMap
parseOsu = const $ Left "not implemented"
