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
  ofs ← offsetP <* char ','
  mpb ← millisecondsPerBeatP <* char ','
  met ← meterP <* char ','
  sat ← sampleTypeP <* char ','
  tim ← timingSampleSetP <* char ','
  vol ← volumeP <* char ','
  inh ← inheritedP <* char ','
  kia ← kiaiModeP <* endOfLine
  return $ TimingPoint { _offset = ofs
                       , _millisecondsPerBeat = mpb
                       , _meter = met
                       , _sampleType = sat
                       , _timingSampleSet = tim
                       , _volume = vol
                       , _inherited = inh
                       , _kiaiMode = kia
                       }

offsetP ∷ Parser Double
offsetP = double

millisecondsPerBeatP ∷ Parser Double
millisecondsPerBeatP = signed double

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
