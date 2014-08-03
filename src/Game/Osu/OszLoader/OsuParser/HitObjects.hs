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

spinnerP ∷ Parser (Int, Int, Int, Int, Int)
spinnerP = do
  n1 ← decimal <* ","
  n2 ← decimal <* ","
  n3 ← decimal <* ","
  n4 ← decimal <* ","
  n5 ← decimal
  return (n1, n2, n3, n4, n5)

circleP ∷ Parser (Int, Int, Int, Int, Int, (Int, Int, Int, Int))
circleP = do
  n1 ← decimal <* ","
  n2 ← decimal <* ","
  n3 ← decimal <* ","
  n4 ← decimal <* ","
  n5 ← decimal <* ","
  n6 ← decimal <* ":"
  n7 ← decimal <* ":"
  n8 ← decimal <* ":"
  n9 ← decimal <* ":"
  return (n1, n2, n3, n4, n5, (n6, n7, n8, n9))

sliderP ∷ Parser (Int, Int, Int, Int, Int, Text)
sliderP = do
  n1 ← decimal <* ","
  n2 ← decimal <* ","
  n3 ← decimal <* ","
  n4 ← decimal <* ","
  n5 ← decimal <* ","
  t ← "B|" *> takeRestOfLine
  return (n1, n2, n3, n4, n5, "B|" `append` t)