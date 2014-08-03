{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Osu.OszLoader.OsuParser.DifficultySpec (spec) where

import Data.Attoparsec.Text
import Data.Char (isSpace)
import Data.Text hiding (map, filter, null)
import Game.Osu.OszLoader.OsuParser.Difficulty
import Game.Osu.OszLoader.Types
import Test.Hspec
import Test.QuickCheck


difficultySample ∷ Text
difficultySample = Data.Text.concat
  [ "[Difficulty]\n"
  , "HPDrainRate:8\n"
  , "CircleSize:4\n"
  , "OverallDifficulty:8\n"
  , "ApproachRate:8\n"
  , "SliderMultiplier:1.8\n"
  , "SliderTickRate:0.5\n"
  ]

numProp p t (Positive x) = parseOnly p tx `shouldBe` Right x
  where tx = t `append` pack (show x)

textProp p t s = parseOnly p tx `shouldBe` Right cs
  where cs = pack $ Prelude.filter (not . isEndOfLine) s
        tx = t `append` cs

spec ∷ Spec
spec = do
  describe "Difficulty section" $ do
    it "can parse the provided sample" $ do
      parseOnly difficultySection difficultySample `shouldBe`
        Right (Difficulty { _hpDrainRate = 8
                          , _circleSize = 4
                          , _overallDifficulty = 8
                          , _approachRate = 8
                          , _sliderMultiplier = 1.8
                          , _sliderTickRate = 0.5})

    context "subparsers" $ do
      it "hpDrainRateP" . property $ numProp hpDrainRateP "HPDrainRate:"

      it "circleSizeP" . property $ numProp circleSizeP "CircleSize:"

      it "overallDifficultyP" . property $
        numProp overallDifficultyP "OverallDifficulty:"

      it "approachRateP" . property $ numProp approachRateP "ApproachRate:"

      it "sliderMultiplierP" . property $
        numProp sliderMultiplierP "SliderMultiplier:"

      it "sliderTickRateP" . property $
        numProp sliderTickRateP "SliderTickRate:"
