{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Osu.OszLoader.OsuParser.EditorSpec (spec) where

import Data.Attoparsec.Text
import Data.Text
import Game.Osu.OszLoader.OsuParser.Editor
import Game.Osu.OszLoader.Types
import Test.Hspec
import Test.QuickCheck



editorSample ∷ Text
editorSample = Data.Text.concat
  [ "[Editor]\n"
  , "Bookmarks: 18297,29658,23365,24758,25289,25696,25925\n"
  , "DistanceSpacing: 1\n"
  , "BeatDivisor: 4\n"
  , "GridSize: 8\n"
  ]


numProp ∷ (Show a, Eq a) ⇒ Parser a → Text → Positive a → Expectation
numProp p t (Positive x) = parseOnly p tx `shouldBe` Right x
  where tx = t `append` pack (show x)

spec ∷ Spec
spec = do
  describe "Editor section" $ do
    it "can parse the provided sample" $ do
      parseOnly editorSection editorSample `shouldBe`
        Right (Editor { _bookmarks = [18297,29658,23365,24758,25289,25696,25925]
                      , _distanceSpacing = 1.0
                      , _beatDivisor = 4
                      , _gridSize = 8
                      , _timelineZoom = Nothing})

    context "subparsers" $ do
      it "bookmarksP" . property $ \xs →
        let ns = Prelude.map getPositive xs
            ys = Prelude.map (pack . show) ns
            zs = intercalate "," ys
        in parseOnly bookmarksP ("Bookmarks: " `append` zs) `shouldBe` Right ns

      it "distanceSpacingP" . property $
        numProp distanceSpacingP "DistanceSpacing: "

      it "beatDivisorP" . property $
        numProp beatDivisorP "BeatDivisor: "

      it "gridSizeP" . property $
        numProp gridSizeP "GridSize: "

      it "timelineZoomP" . property $ numProp timelineZoomP "TimelineZoom: "
