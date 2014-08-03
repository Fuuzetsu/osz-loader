{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Osu.OszLoader.OsuParser.Colours where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Map
import Game.Osu.OszLoader.Types

coloursSection ∷ Parser Colours
coloursSection = do
  _ ← "[Colours]" <* endOfLine
  cs ← many (comboP <* endOfLine)
  return $ Colours { _combo = fromList cs }

comboP ∷ Parser (Int, (Int, Int, Int))
comboP = do
  i ← "Combo" *> decimal <* " : "
  r ← decimal <* ","
  g ← decimal <* ","
  b ← decimal
  return (i, (r, g, b))
