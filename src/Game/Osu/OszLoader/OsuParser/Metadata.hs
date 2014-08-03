{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Osu.OszLoader.OsuParser.Metadata where

import Control.Applicative
import Data.Text
import Data.Attoparsec.Text
import Game.Osu.OszLoader.Types
import Game.Osu.OszLoader.OsuParser.Utils

metadataSection ∷ Parser Metadata
metadataSection = do
  _ ← "[Metadata]" <* endOfLine
  tit ← titleP <* endOfLine
  titUnic ← opt titleUnicodeP
  art ← artistP <* endOfLine
  artUnic ← opt artistUnicodeP
  cr ← creatorP <* endOfLine
  ver ← versionP <* endOfLine
  s ← sourceP <* endOfLine
  ta ← tagsP <* endOfLine
  beatId ← opt beatmapIdP
  beatSetId ← opt beatmapSetIdP
  return $ Metadata { _title = tit
                    , _titleUnicode = titUnic
                    , _artist = art
                    , _artistUnicode = artUnic
                    , _creator = cr
                    , _version = ver
                    , _source = s
                    , _tags = ta
                    , _beatmapId = beatId
                    , _beatmapSetId = beatSetId
                    }

titleP ∷ Parser Text
titleP = "Title:" *> takeRestOfLine

titleUnicodeP ∷ Parser Text
titleUnicodeP = "TitleUnicode:" *> takeRestOfLine

artistP ∷ Parser Text
artistP = "Artist:" *> takeRestOfLine

artistUnicodeP ∷ Parser Text
artistUnicodeP = "ArtistUnicode:" *> takeRestOfLine

creatorP ∷ Parser Text
creatorP = "Creator:" *> takeRestOfLine

versionP ∷ Parser Text
versionP = "Version:" *> takeRestOfLine

sourceP ∷ Parser Text
sourceP = "Source:" *> takeRestOfLine

tagsP ∷ Parser [Text]
tagsP = "Tags:" *> (Data.Text.words <$> takeRestOfLine)

beatmapIdP ∷ Parser Int
beatmapIdP = "BeatmapID:" *> decimal

beatmapSetIdP ∷ Parser Int
beatmapSetIdP = "BeatmapSetID:" *> decimal
