{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Osu.OszLoader.OsuParser.TimingPoint where

import Control.Applicative
import Data.Attoparsec.Text
import Game.Osu.OszLoader.Types
import Game.Osu.OszLoader.OsuParser.Utils

timingPointSection ∷ Parser [TimingPoint]
timingPointSection = "[TimingPoints]" *> endOfLine *> many timingPointP

timingPointP ∷ Parser TimingPoint
timingPointP = do
  ofs ← offsetP <* ","
  mpb ← millisecondsPerBeatP <* ","
  met ← meterP <* ","
  sat ← sampleTypeP <* ","
  tim ← timingSampleSetP <* ","
  vol ← volumeP <* ","
  kia ← kiaiModeP <* ","
  inh ← inheritedP <* endOfLine
  return $ TimingPoint { _offset = ofs
                       , _millisecondsPerBeat = mpb
                       , _meter = met
                       , _sampleType = sat
                       , _timingSampleSet = tim
                       , _volume = vol
                       , _kiaiMode = kia
                       , _inherited = inh
                       }

offsetP ∷ Parser Int
offsetP = decimal

millisecondsPerBeatP ∷ Parser Double
millisecondsPerBeatP = double

meterP ∷ Parser Int
meterP = decimal

sampleTypeP ∷ Parser Int
sampleTypeP = decimal

timingSampleSetP ∷ Parser Int
timingSampleSetP = decimal

volumeP ∷ Parser Int
volumeP = decimal

kiaiModeP ∷ Parser Bool
kiaiModeP = boolIntParser

inheritedP ∷ Parser Bool
inheritedP = boolIntParser
