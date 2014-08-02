{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Osu.OszLoader.OsuParser.GeneralSpec (spec) where

import Data.Attoparsec.Text
import Data.Text
import Game.Osu.OszLoader.OsuParser.Utils
import Game.Osu.OszLoader.Types
import Test.Hspec
import Test.QuickCheck


numProp p t (Positive x) = parseOnly p tx `shouldBe` Right x
  where tx = t `append` pack (show x)

boolProp p t b = parseOnly p tx `shouldBe` Right b
  where tx = t `append` if b then "1" else "0"

textProp p t s = parseOnly p tx `shouldBe` Right cs
  where cs = pack $ Prelude.filter (not . isEndOfLine) s
        tx = t `append` cs

stringProp p t s = parseOnly p (pack tx) `shouldBe` Right cs
  where cs = Prelude.filter (not . isEndOfLine) s
        tx = t ++ cs

maybeBoolProp p t b = parseOnly p tx `shouldBe` r
  where (tx, r) = case b of
          Nothing → ("", Right Nothing)
          Just b' → (t `append` if b' then "1" else "0", Right (Just b'))

spec ∷ Spec
spec = do
  describe "Utils parsers" $ do
    it "takeRestOfLine" . property $ textProp takeRestOfLine ""

    it "boolIntParser" . property $ boolProp boolIntParser ""
