{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Osu.OszLoader.OsuParser.TimingPointSpec (spec) where

import Data.Attoparsec.Text
import Data.Text
import Game.Osu.OszLoader.OsuParser.TimingPoint
import Game.Osu.OszLoader.Types
import Test.Hspec

timingPointSample ∷ Text
timingPointSample = Data.Text.concat
  [ "[TimingPoints]\n"
  , "1187,355.029585798817,4,2,1,100,1,0\n"
  , "6867,-100,4,2,1,100,0,1\n"
  , "18228,-100,4,2,1,100,0,0\n"
  , "63672,-100,4,2,1,100,0,1\n"
  , "89234,-100,4,2,1,100,0,0\n"
  , "160240,-100,4,2,1,100,0,1\n"
  , "160329,-200,4,2,1,75,0,0\n"
  , "166453,-200,4,2,1,43,0,0\n"
  , "166630,-200,4,2,1,75,0,0\n"
  , "167873,-200,4,2,1,43,0,0\n"
  , "168050,-200,4,2,1,75,0,0\n"
  , "171601,-200,4,2,1,81,0,1\n"
  , "171690,-200,4,2,1,75,0,0\n"
  , "185802,-100,4,2,1,100,0,1\n"
  , "208169,-100,4,2,1,100,0,0\n"
  , "208524,-100,4,2,1,100,0,1\n"
  , "234086,-100,4,2,1,100,0,0\n"
  ]


spec ∷ Spec
spec = do
  describe "TimingPoint section" $ do
    it "can parse the provided sample" $ do
      parseOnly timingPointSection timingPointSample `shouldBe`
        Right [ TimingPoint { _offset = 1187
                            , _millisecondsPerBeat = 355.029585798817
                            , _meter = 4
                            , _sampleType = 2
                            , _timingSampleSet = 1
                            , _volume = 100
                            , _kiaiMode = True
                            , _inherited = False}
              , TimingPoint { _offset = 6867
                            , _millisecondsPerBeat = -100.0
                            , _meter = 4
                            , _sampleType = 2
                            , _timingSampleSet = 1
                            , _volume = 100
                            , _kiaiMode = False
                            , _inherited = True}
              , TimingPoint { _offset = 18228
                            , _millisecondsPerBeat = -100.0
                            , _meter = 4
                            , _sampleType = 2
                            , _timingSampleSet = 1
                            , _volume = 100
                            , _kiaiMode = False
                            , _inherited = False}
              , TimingPoint { _offset = 63672
                            , _millisecondsPerBeat = -100.0
                            , _meter = 4
                            , _sampleType = 2
                            , _timingSampleSet = 1
                            , _volume = 100
                            , _kiaiMode = False
                            , _inherited = True}
              , TimingPoint { _offset = 89234
                            , _millisecondsPerBeat = -100.0
                            , _meter = 4
                            , _sampleType = 2
                            , _timingSampleSet = 1
                            , _volume = 100
                            , _kiaiMode = False
                            , _inherited = False}
              , TimingPoint { _offset = 160240
                            , _millisecondsPerBeat = -100.0
                            , _meter = 4
                            , _sampleType = 2
                            , _timingSampleSet = 1
                            , _volume = 100
                            , _kiaiMode = False
                            , _inherited = True}
              , TimingPoint { _offset = 160329
                            , _millisecondsPerBeat = -200.0
                            , _meter = 4
                            , _sampleType = 2
                            , _timingSampleSet = 1
                            , _volume = 75
                            , _kiaiMode = False
                            , _inherited = False}
              , TimingPoint { _offset = 166453
                            , _millisecondsPerBeat = -200.0
                            , _meter = 4
                            , _sampleType = 2
                            , _timingSampleSet = 1
                            , _volume = 43
                            , _kiaiMode = False
                            , _inherited = False}
              , TimingPoint { _offset = 166630
                            , _millisecondsPerBeat = -200.0
                            , _meter = 4
                            , _sampleType = 2
                            , _timingSampleSet = 1
                            , _volume = 75
                            , _kiaiMode = False
                            , _inherited = False}
              , TimingPoint { _offset = 167873
                            , _millisecondsPerBeat = -200.0
                            , _meter = 4
                            , _sampleType = 2
                            , _timingSampleSet = 1
                            , _volume = 43
                            , _kiaiMode = False
                            , _inherited = False}
              , TimingPoint { _offset = 168050
                            , _millisecondsPerBeat = -200.0
                            , _meter = 4
                            , _sampleType = 2
                            , _timingSampleSet = 1
                            , _volume = 75
                            , _kiaiMode = False
                            , _inherited = False}
              , TimingPoint { _offset = 171601
                            , _millisecondsPerBeat = -200.0
                            , _meter = 4
                            , _sampleType = 2
                            , _timingSampleSet = 1
                            , _volume = 81
                            , _kiaiMode = False
                            , _inherited = True}
              , TimingPoint { _offset = 171690
                            , _millisecondsPerBeat = -200.0
                            , _meter = 4
                            , _sampleType = 2
                            , _timingSampleSet = 1
                            , _volume = 75
                            , _kiaiMode = False
                            , _inherited = False}
              , TimingPoint { _offset = 185802
                            , _millisecondsPerBeat = -100.0
                            , _meter = 4
                            , _sampleType = 2
                            , _timingSampleSet = 1
                            , _volume = 100
                            , _kiaiMode = False
                            , _inherited = True}
              , TimingPoint { _offset = 208169
                            , _millisecondsPerBeat = -100.0
                            , _meter = 4
                            , _sampleType = 2
                            , _timingSampleSet = 1
                            , _volume = 100
                            , _kiaiMode = False
                            , _inherited = False}
              , TimingPoint { _offset = 208524
                            , _millisecondsPerBeat = -100.0
                            , _meter = 4
                            , _sampleType = 2
                            , _timingSampleSet = 1
                            , _volume = 100
                            , _kiaiMode = False
                            , _inherited = True}
              , TimingPoint { _offset = 234086
                            , _millisecondsPerBeat = -100.0
                            , _meter = 4
                            , _sampleType = 2
                            , _timingSampleSet = 1
                            , _volume = 100
                            , _kiaiMode = False
                            , _inherited = False}]
