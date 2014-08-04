{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Osu.OszLoader.OsuParser.Colours where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Map
import Game.Osu.OszLoader.Types
import Game.Osu.OszLoader.OsuParser.Utils

coloursSection ∷ Parser Colours
coloursSection = do
  _ ← "[Colours]" <* endOfLine
  cs ← many (comboP <* endOfLine)
  sbs ← opt sliderBorderP
  return $ Colours { _combo = fromList cs
                   , _sliderBorder = sbs
                   }

sliderBorderP ∷ Parser (Int, Int, Int)
sliderBorderP = (,,) <$> ("SliderBorder : " *> decCom) <*> decCom <*> decimal


comboP ∷ Parser (Int, (Int, Int, Int))
comboP = do
  i ← "Combo" *> decimal <* " : "
  rgb ← (,,) <$> decCom <*> decCom <*> decimal
  return (i, rgb)
