{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Osu.OszLoader.OsuParser.Difficulty where

import Control.Applicative
import Data.Attoparsec.Text
import Game.Osu.OszLoader.Types
import Game.Osu.OszLoader.OsuParser.Utils

difficultySection ∷ Parser Difficulty
difficultySection = do
  _ ← "[Difficulty]" <* endOfLine
  hp ← hpDrainRateP <* endOfLine
  cs ← circleSizeP <* endOfLine
  od ← overallDifficultyP <* endOfLine
  ar ← opt approachRateP
  sm ← sliderMultiplierP <* endOfLine
  st ← sliderTickRateP <* endOfLine
  return $ Difficulty { _hpDrainRate = hp
                      , _circleSize = cs
                      , _overallDifficulty = od
                      , _approachRate = ar
                      , _sliderMultiplier = sm
                      , _sliderTickRate = st
                      }

hpDrainRateP ∷ Parser Int
hpDrainRateP = "HPDrainRate:" *> decimal

circleSizeP  ∷ Parser Double
circleSizeP = "CircleSize:" *> double

overallDifficultyP  ∷ Parser Int
overallDifficultyP = "OverallDifficulty:" *> decimal

approachRateP  ∷ Parser Double
approachRateP = "ApproachRate:" *> double

sliderMultiplierP ∷ Parser Double
sliderMultiplierP = "SliderMultiplier:" *> double

sliderTickRateP  ∷ Parser Double
sliderTickRateP = "SliderTickRate:" *> double
