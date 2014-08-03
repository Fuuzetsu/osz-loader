{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Osu.OszLoader.OsuParser.ColoursSpec (spec) where

import Data.Attoparsec.Text
import Data.Map (fromList)
import Data.Text
import Game.Osu.OszLoader.OsuParser.Colours
import Game.Osu.OszLoader.Types
import Test.Hspec
import Test.QuickCheck

coloursSample ∷ Text
coloursSample = Data.Text.concat
  [ "[Colours]\n"
  , "Combo1 : 182,182,182\n"
  , "Combo2 : 255,0,0\n"
  , "Combo3 : 255,132,9\n"
  ]


appendT ∷ Show a => Text -> a -> Text
x `appendT` i = x `append` pack (show i)

spec ∷ Spec
spec = do
  describe "Colours section" $ do
    it "can parse the provided sample" $ do
      parseOnly coloursSection coloursSample `shouldBe`
        Right (Colours {_combo = fromList [ (1, (182,182,182))
                                          , (2, (255,0,0))
                                          , (3, (255, 132, 9))
                                          ]})

    context "subparsers" $ do

      it "comboP" . property $ \(Positive i, Positive r,
                                 Positive g, Positive b) →
        let t = "Combo" `appendT` i `append` " : " `appendT`
                r `append` "," `appendT` g `append` "," `appendT` b
        in parseOnly comboP t `shouldBe` Right (i, (r, g, b))
