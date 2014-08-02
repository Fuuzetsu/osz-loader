{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Osu.OszLoader.OsuParser.General where

import Control.Applicative
import Data.Text
import Data.Attoparsec.Text
import Game.Osu.OszLoader.Types
import Game.Osu.OszLoader.OsuParser.Utils

-- $setup
-- >>> import Control.Applicative
-- >>> import Data.Attoparsec.Text
-- >>> import Data.Text
-- >>> import Test.QuickCheck
-- >>> :set -XOverloadedStrings
-- >>> :set -XUnicodeSyntax

generalSample ∷ Text
generalSample = Data.Text.concat
  [ "[General]\n"
  , "AudioFilename: Mami Kawada - Hishoku no Sora.mp3\n"
  , "AudioLeadIn: 0\n"
  , "PreviewTime: 62962\n"
  , "Countdown: 0\n"
  , "SampleSet: Soft\n"
  , "StackLeniency: 0.5\n"
  , "Mode: 0\n"
  , "LetterboxInBreaks: 1\n"
  ]

-- * Parsers for __General__ section

-- |
-- >>> parseOnly generalSection generalSample
-- Right (General {_audioFilename = "Mami Kawada - Hishoku no Sora.mp3", _audioLeadIn = 0, _previewTime = 62962, _countdown = False, _sampleSet = "Soft", _stackLeniency = 0.5, _mode = 0, _letterboxInBreaks = True, _widescreenStoryboard = Nothing})
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
  lb ← letterboxInBreaksP <* endOfLine
  ws ← widescreenStoryboardP
  return $ General { _audioFilename = af
                   , _audioLeadIn = ali
                   , _previewTime = pt
                   , _countdown = cd
                   , _sampleSet = ss
                   , _stackLeniency = sl
                   , _mode = m
                   , _letterboxInBreaks = lb
                   , _widescreenStoryboard = ws
                   }

-- |
-- >>> parseOnly audioFilenameP "AudioFilename: Hello World!"
-- Right "Hello World!"
audioFilenameP ∷ Parser FilePath
audioFilenameP = "AudioFilename: " *> (unpack <$> takeRestOfLine)

-- |
-- >>> parseOnly audioLeadInP "AudioLeadIn: 17"
-- Right 17
audioLeadInP ∷ Parser Int
audioLeadInP = "AudioLeadIn: " *> decimal

-- |
-- >>> parseOnly previewTimeP "PreviewTime: 16"
-- Right 16
previewTimeP ∷ Parser Int
previewTimeP = "PreviewTime: " *> decimal

-- |
-- >>> parseOnly countdownP "Countdown: 0"
-- Right False
-- >>> parseOnly countdownP "Countdown: 0"
-- Right True
countdownP ∷ Parser Bool
countdownP = "Countdown: " *> boolIntParser

-- |
-- >>> parseOnly sampleSetP "SampleSet: Soft"
-- Right "Soft"
sampleSetP ∷ Parser Text
sampleSetP = "SampleSet: " *> takeRestOfLine

-- |
-- >>> parseOnly stackLeniencyP "StackLeniency: 0.7"
-- Right 0.7
stackLeniencyP ∷ Parser Double
stackLeniencyP = "StackLeniency: " *> double

-- |
-- >>> parseOnly modeP "Mode: 2"
-- Right 2
modeP ∷ Parser Int
modeP = "Mode: " *> decimal

-- |
-- >>> parseOnly inheritedP "Inherited: 0"
-- Right False
inheritedP ∷ Parser Bool
inheritedP = "Inherited: " *> boolIntParser

-- |
-- >>> parseOnly letterboxInBreaksP "LetterboxInBreaks: 1"
-- Right True
letterboxInBreaksP ∷ Parser Bool
letterboxInBreaksP = "LetterboxInBreaks: " *> boolIntParser

-- |
-- >>> parseOnly widescreenStoryboardP "WidescreenStoryboard: 1"
-- Right (Just True)
-- >>> parseOnly widescreenStoryboardP ""
-- Right Nothing
widescreenStoryboardP ∷ Parser (Maybe Bool)
widescreenStoryboardP = "WidescreenStoryboard: " *> (Just <$> boolIntParser)
                        <|> return Nothing
