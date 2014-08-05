{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Osu.OszLoader.OsuParser where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Text hiding (filter, isSuffixOf)
import Game.Osu.OszLoader.OsuParser.Colours
import Game.Osu.OszLoader.OsuParser.Difficulty
import Game.Osu.OszLoader.OsuParser.Editor
import Game.Osu.OszLoader.OsuParser.Events
import Game.Osu.OszLoader.OsuParser.General
import Game.Osu.OszLoader.OsuParser.HitObjects
import Game.Osu.OszLoader.OsuParser.Metadata
import Game.Osu.OszLoader.OsuParser.TimingPoint
import Game.Osu.OszLoader.OsuParser.Utils
import Game.Osu.OszLoader.Types

parseOsu ∷ Text -- ^ .osu file content
         → Maybe Text -- ^ .osb file content if any
         → Either String OsuMap
parseOsu t mt = case parseOnly osuParser t of
  Left m → Left $ "Parsing .osu file content failed with: " ++ m
  Right om → case mt of
    Nothing → Right om
    Just t' → case parseOnly osbParser  t' of
      Left m → Left $ "Parsing .osb file content failed with: " ++ m
      Right (obs, smps) →
        Right om { _events = (_events om) { _storyboardEvents = obs
                                          , _eventSamples = smps
                                          }
                 }

osbParser ∷ Parser ([(EventObject, [EventCommand])], [EventSample])
osbParser = skipSpace *> osbEventsSectionP <* skipSpace <* endOfInput

osuParser ∷ Parser OsuMap
osuParser = do
  fv ← "osu file format v" *> decimal <* skipSpace
  gs ← (generalSection <?> "generalSection") <* skipSpace
  es ← (editorSection  <?> "editorSection") <* skipSpace
  ms ← (metadataSection <?> "metadataSection") <* skipSpace
  ds ← (difficultySection <?> "difficultySection") <* skipSpace
  (back, br, bcs) ← (osuEventsSectionP <?> "osuEventsSectionP") <* skipSpace
  ts ← (timingPointSection <?> "timingPointSection") <* skipSpace
  cs ← opt (coloursSection <?> "coloursSection") <* skipSpace
  hs ← (hitObjectsSection <?> "hitObjectsSection")
  _ ← (skipSpace <* endOfInput) <?> "endOfFile space clear"
  return $ OsuMap { _formatVersion = fv
                  , _general = gs
                  , _editor = es
                  , _metadata = ms
                  , _difficulty = ds
                  , _events = Events { _backgroundEvents = back
                                     , _breakPeriods = br
                                     , _storyboardEvents = []
                                     , _eventSamples = []
                                     , _colourTransformations = bcs
                                     }
                  , _timingPoints = ts
                  , _colours = cs
                  , _hitObjects = hs
                  }
