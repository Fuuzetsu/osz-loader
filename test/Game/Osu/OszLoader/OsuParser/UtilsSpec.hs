{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Osu.OszLoader.OsuParser.UtilsSpec (spec) where

import Data.Attoparsec.Text
import Data.Text
import Game.Osu.OszLoader.OsuParser.Utils
import Test.Hspec
import Test.QuickCheck


boolProp ∷ Parser Bool → Text → Bool → Expectation
boolProp p t b = parseOnly p tx `shouldBe` Right b
  where tx = t `append` if b then "1" else "0"

textProp ∷ Parser Text → Text → [Char] → Expectation
textProp p t s = parseOnly p tx `shouldBe` Right cs
  where cs = pack $ Prelude.filter (not . isEndOfLine) s
        tx = t `append` cs

spec ∷ Spec
spec = do
  describe "Utils parsers" $ do
    it "takeRestOfLine" . property $ textProp takeRestOfLine ""

    it "boolIntParser" . property $ boolProp boolIntParser ""
