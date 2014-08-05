{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Osu.OszLoader.OsuParser.GeneralSpec (spec) where

import Data.Attoparsec.Text
import Data.Text
import Game.Osu.OszLoader.OsuParser.General
import Game.Osu.OszLoader.Types
import Test.Hspec
import Test.QuickCheck


generalSample ∷ Text
generalSample = Data.Text.concat
  [ "[General]\n"
  , "AudioFilename: Mami Kawada - Hishoku no Sora.mp3\n"
  , "AudioLeadIn: 0\n"
  , "PreviewTime: 62962\n"
  , "Countdown: 0\n"
  , "SampleSet: Soft\n"
  , "StackLeniency: 0.5\n"
  , "Mode: 0\n"
  , "LetterboxInBreaks: 1\n"
  ]


numProp ∷ (Show a, Eq a) ⇒ Parser a → Text → Positive a → Expectation
numProp p t (Positive x) = parseOnly p tx `shouldBe` Right x
  where tx = t `append` pack (show x)

boolProp ∷ Parser Bool → Text → Bool → Expectation
boolProp p t b = parseOnly p tx `shouldBe` Right b
  where tx = t `append` if b then "1" else "0"

textProp ∷ Parser Text → Text → [Char] → Expectation
textProp p t s = parseOnly p tx `shouldBe` Right cs
  where cs = pack $ Prelude.filter (not . isEndOfLine) s
        tx = t `append` cs

stringProp ∷ Parser [Char] → [Char] → [Char] → Expectation
stringProp p t s = parseOnly p (pack tx) `shouldBe` Right cs
  where cs = Prelude.filter (not . isEndOfLine) s
        tx = t ++ cs

spec ∷ Spec
spec = do
  describe "General section" $ do
    it "can parse the provided sample" $ do
      parseOnly generalSection generalSample `shouldBe`
        Right (General {_audioFilename = "Mami Kawada - Hishoku no Sora.mp3"
                       , _audioLeadIn = 0
                       , _previewTime = 62962
                       , _countdown = False
                       , _sampleSet = "Soft"
                       , _stackLeniency = 0.5
                       , _mode = 0
                       , _letterboxInBreaks = True
                       , _epilepsyWarning = Nothing
                       , _specialStyle = Nothing
                       , _widescreenStoryboard = Nothing})

    context "subparsers" $ do
      it "audioFilenameP" . property $
        stringProp audioFilenameP "AudioFilename: "

      it "previewTimeP" . property $ numProp previewTimeP "PreviewTime: "

      it "audioLeadInP" . property $ numProp audioLeadInP "AudioLeadIn: "

      it "countdownP" . property $ boolProp countdownP "Countdown: "

      it "samplesetP" . property $ textProp sampleSetP "SampleSet: "

      it "stackLeniencyP" . property $ numProp stackLeniencyP "StackLeniency: "

      it "modeP" . property $ numProp modeP "Mode: "

      it "inheritedP" . property $ boolProp inheritedP "Inherited: "

      it "epilepsyWarningP" . property $
        boolProp epilepsyWarningP "EpilepsyWarning: "

      it "letterboxInBreaksP" . property $
        boolProp letterboxInBreaksP "LetterboxInBreaks: "

      it "specialStyleP" . property $
        boolProp specialStyleP "SpecialStyle: "

      it "widescreenStoryboardP" . property $
        boolProp widescreenStoryboardP "WidescreenStoryboard: "
