{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Osu.OszLoader.OsuParser.Editor where

import Control.Applicative
import Data.Attoparsec.Text
import Game.Osu.OszLoader.Types

editorSection ∷ Parser Editor
editorSection = do
  _ ← "[Editor]" <* endOfLine
  b ← (bookmarksP <* endOfLine) <|> return []
  ds ← distanceSpacingP <* endOfLine
  bd ← beatDivisorP <* endOfLine
  gs ← gridSizeP <* endOfLine
  tz ← (Just <$> timelineZoomP <* endOfLine) <|> return Nothing
  return $ Editor { _bookmarks = b
                  , _distanceSpacing = ds
                  , _beatDivisor = bd
                  , _gridSize = gs
                  , _timelineZoom = tz
                  }

bookmarksP ∷ Parser [Int]
bookmarksP = "Bookmarks: " *> decimal `sepBy` ","

distanceSpacingP ∷ Parser Double
distanceSpacingP = "DistanceSpacing: " *> double

beatDivisorP ∷ Parser Int
beatDivisorP = "BeatDivisor: " *> decimal

gridSizeP ∷ Parser Int
gridSizeP = "GridSize: " *> decimal

timelineZoomP ∷ Parser Int
timelineZoomP = "TimelineZoom: " *> decimal
