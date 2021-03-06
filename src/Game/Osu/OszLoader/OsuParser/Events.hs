{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Game.Osu.OszLoader.OsuParser.Events where

import Data.Text
import Control.Applicative
import Data.Attoparsec.Text hiding (Fail)
import Game.Osu.OszLoader.Types
import Game.Osu.OszLoader.OsuParser.Utils

-- | Parses out the Events section items usually found in .osu files.
-- Storyboarding sections should be parsed separately from .osb files
-- and then the result joined together into 'Events'.
osuEventsSectionP ∷ Parser ([EventBackground],
                            [EventBreakPeriod],
                            [EventsBackgroundColour])
osuEventsSectionP = do
  _ ← "[Events]" <* endOfLine
  (,,) <$> skipComments (backgroundEventP <* endOfLine)
       <*> skipComments (breakP <* endOfLine)
       <*> skipComments (backgroundColourP <* endOfLine)

skipComments ∷ Parser a → Parser [a]
skipComments p = many $ skipping (many commentP) *> p

osbEventsSectionP ∷ Parser ([(EventObject, [EventCommand])], [EventSample])
osbEventsSectionP = do
  _ ← "[Events]" *> endOfLine
  r ← (,) <$> eventsP <*> samplesP
  many commentP *> skipSpace *> endOfInput *> return r

commentP ∷ Parser Text
commentP = "//" *> takeRestOfLine <* skipping endOfLine

eventLayerP ∷ Parser EventLayer
eventLayerP = "Background" *> return Background
              <|> "Fail" *> return Fail
              <|> "Pass" *> return Pass
              <|> "Foreground" *> return Foreground

eventOriginP ∷ Parser EventOrigin
eventOriginP = "TopLeft" *> return TopLeft
               <|> "TopCentre" *> return TopCentre
               <|> "TopRight" *> return TopRight
               <|> "CentreLeft" *> return CentreLeft
               <|> "Centre" *> return Centre
               <|> "CentreRight" *> return CentreRight
               <|> "BottomLeft" *> return BottomLeft
               <|> "BottomCentre" *> return BottomCentre
               <|> "BottomRight" *> return BottomRight

looptypeP ∷ Parser EventLoopType
looptypeP = "Loop" *> ("Forever" *> return LoopForever
                      <|> "Once" *> return LoopOnce)


eventsP ∷ Parser [(EventObject, [EventCommand])]
eventsP = skipComments (singleEventP <|> singleEventP)

commandsP ∷ Parser [Text]
commandsP = many (indentP *> takeRestOfLine <* endOfLine)

singleEventP ∷ Parser (EventObject, [EventCommand])
singleEventP = do
  a ← spriteP <|> movingP
  cs ← endOfLine *> commandsP <|> return []
  return (a, cs)

movingP ∷ Parser EventObject
movingP = do
  l ← "Animation," *> eventLayerP <* char ','
  o ← eventOriginP <* char ','
  fp ← quotedFP <* char ','
  (evx, evy, fc, fd) ← (,,,) <$> decCom <*> decCom <*> decCom <*> decCom
  lt ← looptypeP
  return $ MovingImage { _layer = l
                       , _origin = o
                       , _filePath = fp
                       , _eventX = evx
                       , _eventY = evy
                       , _frameCount = fc
                       , _frameDelay = fd
                       , _looptype = lt
                       }

spriteP ∷ Parser EventObject
spriteP = do
  l ← "Sprite," *> eventLayerP <* char ','
  o ← eventOriginP <* char ','
  fp ← quotedFP <* char ','
  evx ← signed decCom
  evy ← signed decimal
  return $ BasicImage { _layer = l
                      , _origin = o
                      , _filePath = fp
                      , _eventX = evx
                      , _eventY = evy
                      }


samplesP ∷ Parser [EventSample]
samplesP = skipComments (sampleP <* endOfLine)

sampleP ∷ Parser EventSample
sampleP = do
  n1 ← "Sample," *> decCom
  n2 ← decCom
  fp ← quotedFP
  v ← char ',' *> decimal
  return $ EventSample { _evSample1 = n1
                       , _evSample2 = n2
                       , _sampleFilePath = fp
                       , _sampleVolume = v
                       }

indentP ∷ Parser Char
indentP = char '_' <|> char ' '

breakP ∷ Parser (Int, Int, Int)
breakP = (,,) <$> decCom <*> decCom <*> decimal

backgroundEventP ∷ Parser EventBackground
backgroundEventP = video <|> static
  where
    mc = optInline (char ',' *> decimal)
    video = VideoBg <$> ("Video," *> signed decCom) <*> quotedFP
    static = StaticBg <$> decCom <*> decCom <*> quotedFP <*> mc <*> mc

backgroundColourP ∷ Parser (Int, Int, Int, Int, Int)
backgroundColourP =
  (,,,,) <$> decCom <*> decCom <*> decCom <*> decCom <*> decimal
