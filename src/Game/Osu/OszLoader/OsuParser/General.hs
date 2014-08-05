{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Osu.OszLoader.OsuParser.General where

import Control.Applicative
import Data.Text
import Data.Attoparsec.Text
import Game.Osu.OszLoader.Types
import Game.Osu.OszLoader.OsuParser.Utils

generalSection ∷ Parser General
generalSection = do
  _ ← "[General]" <* endOfLine
  af ← audioFilenameP <* endOfLine
  ali ← audioLeadInP <* endOfLine
  pt ← previewTimeP <* endOfLine
  cd ← countdownP <* endOfLine
  ss ← sampleSetP <* endOfLine
  sl ← stackLeniencyP <* endOfLine
  m ← modeP <* endOfLine
  ep ← opt epilepsyWarningP
  lb ← letterboxInBreaksP <* endOfLine
  sp ← opt specialStyleP
  ws ← opt widescreenStoryboardP
  return $ General { _audioFilename = af
                   , _audioLeadIn = ali
                   , _previewTime = pt
                   , _countdown = cd
                   , _sampleSet = ss
                   , _stackLeniency = sl
                   , _mode = m
                   , _letterboxInBreaks = lb
                   , _epilepsyWarning = ep
                   , _specialStyle = sp
                   , _widescreenStoryboard = ws
                   }

audioFilenameP ∷ Parser FilePath
audioFilenameP = "AudioFilename: " *> (unpack <$> takeRestOfLine)

audioLeadInP ∷ Parser Int
audioLeadInP = "AudioLeadIn: " *> decimal

previewTimeP ∷ Parser Int
previewTimeP = "PreviewTime: " *> signed decimal

countdownP ∷ Parser Bool
countdownP = "Countdown: " *> boolIntParser

sampleSetP ∷ Parser Text
sampleSetP = "SampleSet: " *> takeRestOfLine

stackLeniencyP ∷ Parser Double
stackLeniencyP = "StackLeniency: " *> double

modeP ∷ Parser Int
modeP = "Mode: " *> decimal

inheritedP ∷ Parser Bool
inheritedP = "Inherited: " *> boolIntParser

epilepsyWarningP ∷ Parser Bool
epilepsyWarningP = "EpilepsyWarning: " *> boolIntParser

specialStyleP ∷ Parser Bool
specialStyleP = "SpecialStyle: " *> boolIntParser

letterboxInBreaksP ∷ Parser Bool
letterboxInBreaksP = "LetterboxInBreaks: " *> boolIntParser

widescreenStoryboardP ∷ Parser Bool
widescreenStoryboardP = "WidescreenStoryboard: " *> boolIntParser
