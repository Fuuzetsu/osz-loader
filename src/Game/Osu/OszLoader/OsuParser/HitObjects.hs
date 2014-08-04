{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Game.Osu.OszLoader.OsuParser.HitObjects where

import Data.Text
import Control.Applicative
import Data.Attoparsec.Text
import Game.Osu.OszLoader.Types
import Game.Osu.OszLoader.OsuParser.Utils

hitObjectsSection ∷ Parser [HitObject]
hitObjectsSection = do
  _ ← "[HitObjects]" <* endOfLine
  many (hitObjectP <* endOfLine)


hitObjectP ∷ Parser HitObject
hitObjectP = Slider <$> sliderP
             <|> Circle <$> circleP
             <|> Spinner <$> spinnerP

spinnerP ∷ Parser (Int, Int, Int, Int, Int, Maybe Int)
spinnerP = do
  n1 ← decCom
  n2 ← decCom
  n3 ← decCom
  n4 ← decCom
  n5 ← decimal
  n6 ← optInline (char ',' *> decimal)
  return (n1, n2, n3, n4, n5, n6)

circleP ∷ Parser (Int, Int, Int, Int, Int, (Int, Int, Int, Int))
circleP = do
  n1 ← decCom
  n2 ← decCom
  n3 ← decCom
  n4 ← decCom
  n5 ← decCom
  n6 ← decimal <* char ':'
  n7 ← decimal <* char ':'
  n8 ← decimal <* char ':'
  n9 ← decimal <* char ':'
  return (n1, n2, n3, n4, n5, (n6, n7, n8, n9))

sliderP ∷ Parser (Int, Int, Int, Int, Int, Text)
sliderP = do
  n1 ← decCom
  n2 ← decCom
  n3 ← decCom
  n4 ← decCom
  n5 ← decCom
  (c, t) ← (,) <$> curvePrefix <*> takeRestOfLine
  return (n1, n2, n3, n4, n5, c `cons` '|' `cons` t)
  where
    curvePrefix = (char 'B' <|> char 'L' <|> char 'P') <* char '|'
