{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Osu.OszLoader.OsuParser.Utils where

import Control.Applicative
import Data.Text
import Data.Attoparsec.Text

opt ∷ Parser a → Parser (Maybe a)
opt p = (Just <$> p <* endOfLine) <|> return Nothing

optInline ∷ Parser a → Parser (Maybe a)
optInline p = (Just <$> p) <|> return Nothing

takeRestOfLine ∷ Parser Text
takeRestOfLine = takeTill isEndOfLine

boolIntParser ∷ Parser Bool
boolIntParser = char '0' *> return False
                <|>
                decimal *> return True

skipping ∷ Parser a → Parser ()
skipping p = (p *> return ()) <|> return ()

quotedFP ∷ Parser FilePath
quotedFP = unpack <$> (char '"' *> takeTill (== '"') <* char '"')

decCom ∷ Parser Int
decCom = decimal <* char ','
