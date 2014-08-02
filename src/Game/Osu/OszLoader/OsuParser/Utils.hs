{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Osu.OszLoader.OsuParser.Utils where

import Control.Applicative
import Data.Text
import Data.Attoparsec.Text

takeRestOfLine ∷ Parser Text
takeRestOfLine = takeTill isEndOfLine

boolIntParser ∷ Parser Bool
boolIntParser = "0" *> return False
                <|> "1" *> return True
