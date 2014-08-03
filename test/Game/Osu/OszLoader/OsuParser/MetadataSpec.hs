{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Osu.OszLoader.OsuParser.MetadataSpec (spec) where

import Data.Attoparsec.Text
import Data.Char (isSpace)
import Data.Text hiding (map, filter, null)
import Game.Osu.OszLoader.OsuParser.Metadata
import Game.Osu.OszLoader.Types
import Test.Hspec
import Test.QuickCheck


metadataSample ∷ Text
metadataSample = Data.Text.concat
  [ "[Metadata]\n"
  , "Title:Hishoku no Sora\n"
  , "Artist:Kawada Mami\n"
  , "Creator:lkp\n"
  , "Version:Frantic\n"
  , "Source:Shakugan no Shana\n"
  , "Tags:koko6162006 Muya Lunet S_o_r_d_a\n"
  ]

numProp ∷ (Show a, Eq a) ⇒ Parser a → Text → Positive a → Expectation
numProp p t (Positive x) = parseOnly p tx `shouldBe` Right x
  where tx = t `append` pack (show x)

textProp ∷ Parser Text → Text → [Char] → Expectation
textProp p t s = parseOnly p tx `shouldBe` Right cs
  where cs = pack $ Prelude.filter (not . isEndOfLine) s
        tx = t `append` cs

spec ∷ Spec
spec = do
  describe "Metadata section" $ do
    it "can parse the provided sample" $ do
      parseOnly metadataSection metadataSample `shouldBe`
        Right (Metadata { _title = "Hishoku no Sora"
                        , _titleUnicode = Nothing
                        , _artist = "Kawada Mami"
                        , _artistUnicode = Nothing
                        , _creator = "lkp"
                        , _version = "Frantic"
                        , _source = "Shakugan no Shana"
                        , _tags = ["koko6162006","Muya","Lunet","S_o_r_d_a"]
                        , _beatmapId = Nothing
                        , _beatmapSetId = Nothing})

    context "subparsers" $ do
      it "titleP" . property $ textProp titleP "Title:"

      it "titleUnicodeP" . property $ textProp titleUnicodeP "TitleUnicode:"

      it "artistP" . property $ textProp artistP "Artist:"

      it "creatorP" . property $ textProp creatorP "Creator:"

      it "versionP" . property $ textProp versionP "Version:"

      it "sourceP" . property $ textProp sourceP "Source:"

      it "tagsP" . property $ \xs →
        let ws x = isEndOfLine x || isSpace x
            ns ∷ [String]
            ns = filter (not . null) $ map (filter (not . ws)) xs

            ys ∷ [Text]
            ys = map pack ns

            zs ∷ Text
            zs = Data.Text.unwords ys
        in parseOnly tagsP ("Tags:" `append` zs) `shouldBe` Right ys

      it "beatmapIdP" . property $ numProp beatmapIdP "BeatmapID:"

      it "beatmapSetIdP" . property $ numProp beatmapSetIdP "BeatmapSetID:"
