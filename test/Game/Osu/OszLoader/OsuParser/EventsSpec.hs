{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Osu.OszLoader.OsuParser.EventsSpec (spec) where

import Data.Attoparsec.Text hiding (Fail)
import Data.Text hiding (filter, map)
import Game.Osu.OszLoader.OsuParser.Events
import Game.Osu.OszLoader.Types
import Test.Hspec
import Test.QuickCheck

appendT ∷ Text -> Int -> Text
x `appendT` i = x `append` pack (show i)

newtype FP = FP String deriving (Show, Eq)

instance Arbitrary FP where
  arbitrary = do
    s ← arbitrary
    let fp = filter (\x → x /= '"' && x /= '\n') s
    return $ FP fp



spec ∷ Spec
spec = do
  describe "Events section" $ do
    it "can parse sample Sprite" $ do
      let t = "Sprite,Background,TopLeft,"
              `append` "\"Captured\\video.mp4_000010266.jpg\",0,-480\n"
      parseOnly eventsP t `shouldBe`
        Right [ (BasicImage { _layer = Background
                            , _origin = TopLeft
                            , _filePath = "Captured\\video.mp4_000010266.jpg"
                            , _eventX = 0, _eventY = -480 }, [])
              ]

    it "can parse sample Animation" $ do
      let t = "Animation,Fail,Centre,\"Captured\\NS-.jpg\""
              `append` ",320,240,4,300,LoopForever"
      parseOnly eventsP t `shouldBe`
        Right [ (MovingImage { _layer = Fail
                             , _origin = Centre
                             , _filePath = "Captured\\NS-.jpg"
                             , _eventX = 320
                             , _eventY = 240
                             , _frameCount = 4
                             , _frameDelay = 300
                             , _looptype = LoopForever
                             }, [])
              ]

    it "can parse sample Animation with commands" $ do
      parseOnly eventsP aniCommands `shouldBe`
        Right [ (MovingImage { _layer = Pass
                             , _origin = Centre
                             , _filePath = "Captured\\S3-.jpg"
                             , _eventX = 320
                             , _eventY = 240
                             , _frameCount = 4
                             , _frameDelay = 150
                             , _looptype = LoopForever },
                 [ "M,0,128329,,320,240", "F,0,128329,182850,1"])]

    it "can parse multiple commands" $ do
      parseOnly commandsP commands `shouldBe`
        Right [ "F,0,108361,,1"
              , "M,0,108361,108906,680,326,540,224"
              , "R,0,108361,108906,0,0.3891199"
              , "M,0,108906,109247,540,224,479,128"
              , "M,0,109247,109520,479,128,439,65"
              , "R,0,108906,109792,0.3891199,0"
              , "M,0,109520,109792,439,65,389,111"
              , "M,0,109792,110883,389,111,144,-71"
              ]

    it "can parse sample Samples" $ do
      parseOnly samplesP samples `shouldBe`
        Right [ EventSample { _evSample1 = 63450
                            , _evSample2 = 2
                            , _sampleFilePath = "pass.mp3"
                            , _sampleVolume = 100 }
              , EventSample { _evSample1 = 115244
                            , _evSample2 = 2
                            , _sampleFilePath = "pass.mp3"
                            , _sampleVolume = 100 }
              , EventSample { _evSample1 = 196480
                            , _evSample2 = 2
                            , _sampleFilePath = "pass.mp3"
                            , _sampleVolume = 100 }
              , EventSample { _evSample1 = 63450
                            , _evSample2 = 1
                            , _sampleFilePath = "fail.mp3"
                            , _sampleVolume = 100 }
              , EventSample { _evSample1 = 115244
                            , _evSample2 = 1
                            , _sampleFilePath = "fail.mp3"
                            , _sampleVolume = 100 }
              , EventSample { _evSample1 = 196480
                            , _evSample2 = 1
                            , _sampleFilePath = "fail.mp3"
                            , _sampleVolume = 100 }
              ]

    context "subparsers" $ do
      it "sampleP" . property $ \(Positive x, Positive y, Positive z, FP s) →
        let fp = '"' : s ++ "\""
            t = "Sample," `appendT` x `append` "," `appendT` y `append` ","
                `append` pack fp `append` "," `appendT` z
        in parseOnly sampleP t `shouldBe`
             Right (EventSample { _evSample1 = x
                                , _evSample2 = y
                                , _sampleFilePath = s
                                , _sampleVolume = z})

    it "backgroundEventP" $ do
      parseOnly backgroundEventP "0,0,\"bg.jpg\",0,0" `shouldBe`
        Right (StaticBg 0 0 "bg.jpg" (Just 0) (Just 0))

      parseOnly backgroundEventP "0,0,\"bg.jpg\"" `shouldBe`
        Right (StaticBg 0 0 "bg.jpg" Nothing Nothing)

    it "can parse provided storyboard part" $ do
      parseOnly eventsP storyboardSample `shouldBe`
        Right [(BasicImage { _layer = Background
                           , _origin = Centre
                           , _filePath = "bg.png"
                           , _eventX = 320
                           , _eventY = 240},[ "F,0,0,,0" ])
              , (BasicImage { _layer = Background
                            , _origin = TopLeft
                            , _filePath = "Captured\\video.mp4_000010266.jpg"
                            , _eventX = 0
                            , _eventY = 480},[ "M,0,5590,7498,0,480,0,-480"
                                             , "F,0,5590,7498,1"
                                             , "M,0,7498,,0,-480"
                                             , "F,0,7498,8724,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\white.png"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,61405,61678,1"
                                             , "F,0,61678,63450,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\white.png"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,112791,115244,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\white.png"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,5317,5590,0,1"
                                             , "M,0,5590,,320,240"
                                             , "F,0,5590,6612,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000011733.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,8724,10292,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000012333.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,10292,11110,1"
                                             , "F,0,11110,11655,1"
                                             , "F,0,11655,12200,1"
                                             , "F,0,12200,13291,1"
                                             , "F,0,13291,14449,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000012966.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,11110,11655,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000012966.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,12200,12745,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\white.png"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,14449,15131,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000015433.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,15131,15744,1"
                                             , "F,0,15744,16153,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000016433.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,16153,17175,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000017233.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,17175,18197,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000017666.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,18197,18606,1"
                                             , "F,0,18606,18811,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000018033.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,18811,19356,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000018333.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,19356,19697,1" ])
              , (BasicImage { _layer = Background
                            , _origin = TopLeft
                            , _filePath = "Captured\\video.mp4_000018933.jpg"
                            , _eventX = -640
                            , _eventY = 0},[ "F,0,19697,20242,1"
                                           , "M,0,20242,21469,-640,0,0,0"
                                           , "F,0,20242,21537,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000021200.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,21537,21673,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000021700.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,21673,22355,1"
                                             , "F,0,22355,23377,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000023733.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,23377,29102,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\Blackbg.png"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,24740,29102,0.4163199" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000030333.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "M,0,28556,,320,240"
                                             , "F,0,28556,29102,0,1"
                                             , "F,0,29102,32373,1" ])
              , (BasicImage { _layer = Background
                            , _origin = TopLeft
                            , _filePath = "Captured\\video.mp4_000050766.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "M,0,49819,50365,-640,0,0,0"
                                             , "F,0,50365,50433,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000050933.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,50433,50569,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000051633.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,50569,50842,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000051633.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,50569,50842,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000051933.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,50842,51114,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000052266.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,51114,51319,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000052433.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,51319,51591,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000052266.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,51591,52137,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000053100.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "M,0,52137,,320,240"
                                             , "F,0,52137,55067,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000055933.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,55067,58883,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000059500.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,58883,59224,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000059666.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,59224,59497,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000060000.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,59497,59769,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000060166.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,59769,60042,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000060433.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,60042,60315,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000060700.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,60315,60587,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000060433.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,60587,60860,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000060700.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,60860,61132,1"
                                             , "F,0,61132,,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000060433.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,61132,61405,1"
                                             , "F,0,61405,,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000030333.jpg"
                            , _eventX = 320
                            , _eventY = 240 }, [ "F,0,64540,65630,0,1"
                                               , "F,0,65630,68902,1"
                                               , "M,0,68902,,320,240"
                                               , "F,0,68902,69447,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000068333.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,63450,64540,1"
                                             , "F,0,64540,65085,1,0" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000086566.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,104340,106521,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000088466.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "M,0,106521,106794,320,240"
                                             ,"F,0,106521,106794,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000088633.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,106794,107134,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000089966.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,107134,107611,1"
                                             ,"F,0,107611,108157,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000090900.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,108157,109792,1"
                                             ,"F,0,109792,110883,1"
                                             ,"M,0,110883,,320,240" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000104200.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "R,0,110883,,0"
                                             ,"F,0,110883,111837,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000104833.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,111837,112518,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000106066.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,112518,112791,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000108400.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,115244,117425,1","F,0,117425,,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000110933.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,117425,118515,1","F,0,118515,119061,1,0","F,0,119061,,0" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000115433.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,118515,119061,0,1","F,0,119061,119606,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000118200.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,119606,119878,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000118633.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,119878,120151,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000119300.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,120151,120424,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000120233.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,120424,120696,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000120466.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,120696,120969,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000120800.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,120969,121242,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000120966.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,121242,121514,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000121100.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,121514,121787,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000121233.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,121787,122059,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000121433.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,122059,122605,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000123266.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,122605,123968,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000030333.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,123695,123968,0,1"
                                             ,"F,0,123968,128329,1,0.992674" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000181300.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "M,0,182850,183395,320,240"
                                             ,"F,0,182850,183395,1"
                                             ,"F,0,183395,183668,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000182700.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,183668,184485,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000183766.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,184485,185031,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000184333.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,185031,185849,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\white.png"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,185849,186462,1"
                                             ,"F,0,186462,187212,1,0" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000185466.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,186462,186939,0,0.636"
                                             ,"F,0,186939,187212,0.636,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\white.png"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,187212,187757,1,0" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000187200.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,187552,,0"
                                             ,"F,0,187552,187893,0,1"
                                             ,"F,0,187893,188847,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\white.png"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,188847,189392,1,0" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000190800.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,188847,189392,0,1"
                                             ,"F,0,189392,191573,1"
                                             ,"F,0,191573,191709,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000193033.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,191709,193550,1"
                                             ,"F,0,193550,193686,1,0.3333333"
                                             ,"F,0,193686,193754,0.3333333,0" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000196000.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,195526,200024,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000200366.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,200024,200569,0,1"
                                             ,"F,0,200569,201114,1"
                                             ,"F,0,201114,201659,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\white.png"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,201659,202205,1,0" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000202500.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,201659,202205,0,1"
                                             ,"F,0,202205,203568,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\white.png"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,203568,204113,1,0" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000204833.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,203568,205612,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\white.png"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,205612,206157,1,0" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000206733.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,205612,207793,1"
                                             ,"F,0,207793,207929,1,0" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000208566.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,207793,207929,0,1"
                                             ,"F,0,207929,208066,1"
                                             ,"F,0,208066,209020,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000209933.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,209020,209565,1"
                                             ,"F,0,209565,210110,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000210233.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,210110,210928,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000211666.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,210928,211473,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000212266.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "M,0,211473,,320,240"
                                             ,"F,0,211473,213109,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\white.png"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,213109,213381,1"
                                             ,"F,0,213381,213654,1,0" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000213700.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,213381,213654,0,1"
                                             ,"F,0,213654,215562,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000216766.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,215562,217470,1" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000217633.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,217470,218288,1,0" ])
              , (BasicImage { _layer = Background
                            , _origin = Centre
                            , _filePath = "Captured\\white.png"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,200024,200569,1,0" ])
              , (MovingImage { _layer = Fail
                             , _origin = Centre
                             , _filePath = "Captured\\NS-.jpg"
                             , _eventX = 320
                             , _eventY = 240
                             , _frameCount = 4
                             , _frameDelay = 300
                             , _looptype = LoopForever},[ "F,0,32373,49819,1" ])
              , (MovingImage { _layer = Fail
                             , _origin = Centre
                             , _filePath = "Captured\\NS-.jpg"
                             , _eventX = 320
                             , _eventY = 240
                             , _frameCount = 4
                             , _frameDelay = 300
                             , _looptype = LoopForever},[ "F,0,69447,104340,1" ])
              , (MovingImage { _layer = Fail
                             , _origin = Centre
                             , _filePath = "Captured\\NS-.jpg"
                             , _eventX = 320
                             , _eventY = 240
                             , _frameCount = 4
                             , _frameDelay = 300
                             , _looptype = LoopForever},[ "F,0,128329,182850,1" ])
              , (BasicImage { _layer = Fail
                            , _origin = Centre
                            , _filePath = "Captured\\SmallChen1.png"
                            , _eventX = 320
                            , _eventY = 240 },
                 [ "F,0,108361,,1" ,"M,0,108361,108906,680,326,540,224"
                 , "R,0,108361,108906,0,0.3891199"
                 , "M,0,108906,109247,540,224,479,128"
                 , "R,0,108906,109520,0.3891199"
                 , "M,0,109247,109520,479,128,439,65"
                 , "M,0,109520,,439,65"
                 , "M,0,109520,110883,439,65,723,252"
                 , "R,0,109520,110883,0.3891199,14.15168" ])
              , (BasicImage { _layer = Fail
                            , _origin = Centre
                            , _filePath = "Captured\\ChenDiagonal.png"
                            , _eventX = 320
                            , _eventY = 240 },
                 [ "M,0,113881,114971,187,-131,741,272"
                 , "R,0,113881,114971,0,7.82336"
                 , "M,0,114971,114972,741,272,758,293" ])
              , (MovingImage { _layer = Fail
                             , _origin = Centre
                             , _filePath = "Captured\\NS-.jpg"
                             , _eventX = 320
                             , _eventY = 240
                             , _frameCount = 4
                             , _frameDelay = 300
                             , _looptype = LoopForever },
                 [ "F,0,217743,218288,0,1","F,0,218288,241732,1" ])
              , (BasicImage { _layer = Fail
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000245466.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,251001,251273,0,1"
                                             ,"F,0,251273,252636,1"
                                             ,"F,0,252636,256998,1"
                                             ,"F,0,256998,259179,1" ])
              , (BasicImage { _layer = Fail
                            , _origin = Centre
                            , _filePath = "Captured\\MISSIONFAILEO.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,193550,193754,0,1"
                                             ,"F,0,193754,195526,1" ])
              , (BasicImage { _layer = Fail
                            , _origin = Centre
                            , _filePath = "fail.png"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,63450,63586,1,0"
                                             ,"F,0,63586,63722,0,1"
                                             ,"F,0,63722,65085,1"
                                             ,"F,0,65085,65222,1,0" ])
              , (BasicImage { _layer = Fail
                            , _origin = Centre
                            , _filePath = "fail.png"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,115244,115381,1,0"
                                             ,"F,0,115381,115517,0,1"
                                             ,"F,0,115517,116880,1"
                                             ,"F,0,116880,117425,1"
                                             ,"F,0,117425,,1" ])
              , (BasicImage { _layer = Fail
                            , _origin = Centre
                            , _filePath = "fail.png"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,196480,196548,1,0"
                                             ,"F,0,196548,196616,0,1"
                                             ,"F,0,196616,198116,1"
                                             ,"F,0,198116,198252,1,0" ])
              , (BasicImage { _layer = Pass
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000194100.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,193550,193754,0,1"
                                             ,"F,0,193754,195526,1" ])
              , (MovingImage { _layer = Pass
                             , _origin = Centre
                             , _filePath = "Captured\\MS.jpg"
                             , _eventX = 320
                             , _eventY = 240
                             , _frameCount = 4
                             , _frameDelay = 150
                             , _looptype = LoopForever},[ "F,0,32373,49819,1" ])
              , (BasicImage { _layer = Pass
                            , _origin = Centre
                            , _filePath = "Captured\\ChenDiagonal.png"
                            , _eventX = 320
                            , _eventY = 240},
                 [ "F,0,60575,60724,0.86176,1"
                 , "M,0,60724,62359,737,309,189,-106" ])
              , (MovingImage { _layer = Pass
                             , _origin = Centre
                             , _filePath = "Captured\\BS-.jpg"
                             , _eventX = 320
                             , _eventY = 240
                             , _frameCount = 4
                             , _frameDelay = 150
                             , _looptype = LoopForever},[ "M,0,69447,,320,240"
                                                        ,"F,0,69447,104340,1" ])
              , (MovingImage { _layer = Pass
                             , _origin = Centre
                             , _filePath = "Captured\\S3-.jpg"
                             , _eventX = 320
                             , _eventY = 240
                             , _frameCount = 4
                             , _frameDelay = 150
                             , _looptype = LoopForever},
                 [ "M,0,128329,,320,240" ,"F,0,128329,182850,1" ])
              , (BasicImage { _layer = Pass
                            , _origin = Centre
                            , _filePath = "Captured\\SmallChen1.png"
                            , _eventX = 320
                            , _eventY = 240},
                 [ "F,0,108361,,1","M,0,108361,108906,680,326,540,224"
                 , "R,0,108361,108906,0,0.3891199"
                 , "M,0,108906,109247,540,224,479,128"
                 , "M,0,109247,109520,479,128,439,65"
                 , "R,0,108906,109792,0.3891199,0"
                 , "M,0,109520,109792,439,65,389,111"
                 , "M,0,109792,110883,389,111,144,-71" ])
              , (MovingImage { _layer = Pass
                             , _origin = Centre
                             , _filePath = "Captured\\ARGH.jpg"
                             , _eventX = 320
                             , _eventY = 240
                             , _frameCount = 4
                             , _frameDelay = 100
                             , _looptype = LoopForever },
                 [ "F,0,217743,218288,0,1","F,0,218288,241732,1" ])
              , (MovingImage { _layer = Pass
                             , _origin = Centre
                             , _filePath = "Captured\\YuyuYoumu.png"
                             , _eventX = 320
                             , _eventY = 240
                             , _frameCount = 2
                             , _frameDelay = 500
                             , _looptype = LoopForever },
                 [ "F,0,217743,218288,0,0.5"
                 , "M,0,218288,,544,59"
                 , "S,0,218288,,0.7952"
                 , "F,0,218288,241732,0.5" ])
              , (MovingImage { _layer = Pass
                             , _origin = Centre
                             , _filePath = "Captured\\MystAli.png"
                             , _eventX = 320
                             , _eventY = 240
                             , _frameCount = 2
                             , _frameDelay = 500
                             , _looptype = LoopForever},
                 [ "M,0,217743,,124,66"
                 ,"S,0,217743,,0.8259199"
                 ,"F,0,217743,218288,0,0.5"
                 ,"F,0,218288,241732,0.5" ])
              , (MovingImage { _layer = Pass
                             , _origin = Centre
                             , _filePath = "Captured\\RiguRumi.png"
                             , _eventX = 320
                             , _eventY = 240
                             , _frameCount = 2
                             , _frameDelay = 500
                             , _looptype = LoopForever},
                 [ "M,0,217743,,115,384","F,0,217743,218288,0,0.5"
                 ,"F,0,218288,241732,0.5" ])
              , (MovingImage { _layer = Pass
                             , _origin = Centre
                             , _filePath = "Captured\\Mari.png"
                             , _eventX = 320
                             , _eventY = 240
                             , _frameCount = 2
                             , _frameDelay = 500
                             , _looptype = LoopForever},
                 [ "M,0,217743,,537,385" ,"F,0,217743,218288,0,0.5"
                 ,"F,0,218288,241732,0.5" ])
              , (BasicImage { _layer = Pass
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000248266.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,251001,251273,0,1"
                                             ,"F,0,251273,252636,1"
                                             ,"F,0,252636,259179,1" ])
              , (BasicImage { _layer = Pass
                            , _origin = Centre
                            , _filePath = "pass.png"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,63450,63586,1,0"
                                             ,"F,0,63586,63722,0,1"
                                             ,"F,0,63722,65085,1"
                                             ,"F,0,65085,65222,1,0" ])
              , (BasicImage { _layer = Pass
                            , _origin = Centre
                            , _filePath = "pass.png"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,115244,115381,1,0"
                                             ,"F,0,115381,115517,0,1"
                                             ,"F,0,115517,116880,1"
                                             ,"F,0,116880,117425,1"
                                             ,"F,0,117425,,1" ])
              , (BasicImage { _layer = Pass
                            , _origin = Centre
                            , _filePath = "pass.png"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,196480,196548,1,0"
                                             ,"F,0,196548,196616,0,1"
                                             ,"F,0,196616,198116,1"
                                             ,"F,0,198116,198252,1,0" ])
              , (BasicImage { _layer = Foreground
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000107700.png"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,112791,113063,1" ])
              , (BasicImage { _layer = Foreground
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000107866.png"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,113063,113336,1" ])
              , (BasicImage { _layer = Foreground
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000108000.png"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,113336,113609,1" ])
              , (BasicImage { _layer = Foreground
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000108133.png"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,113609,113881,1" ])
              , (BasicImage { _layer = Foreground
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000108266.png"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,113881,114154,1"
                                             ,"F,0,114154,115244,1" ])
              , (BasicImage { _layer = Foreground
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000060700.png"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,61405,61678,1"
                                             ,"F,0,61678,63450,1" ])
              , (BasicImage { _layer = Foreground
                            , _origin = TopLeft
                            , _filePath = "Captured\\BikeSide.png"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,30737,,0.77472"
                                             ,"M,0,30737,32237,640,0,-640,0" ])
              , (BasicImage { _layer = Foreground
                            , _origin = TopLeft
                            , _filePath = "Captured\\BikeSide.png"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,67266,,1"
                                             ,"M,0,67266,68902,640,0,-640,0" ])
              , (BasicImage { _layer = Foreground
                            , _origin = Centre
                            , _filePath = "Captured\\YEEEEEEEEEEEAH.png"
                            , _eventX = 320
                            , _eventY = 240},[ "M,0,24740,,322,262"
                                             ,"F,0,24740,25285,0,1"
                                             ,"F,0,25285,28556,1"
                                             ,"F,0,28556,29102,1,0" ])
              , (BasicImage { _layer = Foreground
                            , _origin = Centre
                            , _filePath = "Captured\\ChenDiagonal.png"
                            , _eventX = 320
                            , _eventY = 240},
                 [ "F,0,104613,,1" ,"M,0,104613,105976,685,466,-38,-78" ])
              , (BasicImage { _layer = Foreground
                            , _origin = TopLeft
                            , _filePath = "Captured\\BikeSide.png"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,126966,,1"
                                             ,"M,0,126966,128057,640,0,-640,0"
                                             ])
              , (BasicImage { _layer = Foreground
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000229333.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,242550,243368,1" ])
              , (BasicImage { _layer = Foreground
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000229800.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,243368,243709,1" ])
              , (BasicImage { _layer = Foreground
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000231333.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,243709,244458,1" ])
              , (BasicImage { _layer = Foreground
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000233466.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,244458,245003,1" ])
              , (BasicImage { _layer = Foreground
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000235233.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,245003,245549,1" ])
              , (BasicImage { _layer = Foreground
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000236400.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,245549,246094,1" ])
              , (BasicImage { _layer = Foreground
                            , _origin = Centre
                            , _filePath = "Captured\\white.png"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,247184,247457,1,0" ])
              , (BasicImage { _layer = Foreground
                            , _origin = Centre
                            , _filePath = "Captured\\white.png"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,248275,248547,1,0" ])
              , (BasicImage { _layer = Foreground
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000241233.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,248275,248547,0,1"
                                             ,"F,0,248547,249365,1" ])
              , (BasicImage { _layer = Foreground
                            , _origin = Centre
                            , _filePath = "Captured\\white.png"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,249365,249638,1,0" ])
              , (BasicImage { _layer = Foreground
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000237266.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,246094,246639,1"
                                             ,"F,0,246639,246980,1"
                                             ,"F,0,246980,247184,1" ])
              , (BasicImage { _layer = Foreground
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000239600.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,247184,247457,0,1"
                                             ,"F,0,247457,248275,1" ])
              , (BasicImage { _layer = Foreground
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000242300.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,249365,249638,0,1"
                                             ,"F,0,249638,249910,1" ])
              , (BasicImage { _layer = Foreground
                            , _origin = Centre
                            , _filePath = "Captured\\white.png"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,249910,250183,1,0" ])
              , (BasicImage { _layer = Foreground
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000243400.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,249910,250183,0,1"
                                             ,"F,0,250183,250728,1" ])
              , (BasicImage { _layer = Foreground
                            , _origin = Centre
                            , _filePath = "Captured\\white.png"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,250728,251001,1"
                                             ,"F,0,251001,251273,1,0" ])
              , (BasicImage { _layer = Foreground
                            , _origin = Centre
                            , _filePath = "Captured\\video.mp4_000225333.jpg"
                            , _eventX = 320
                            , _eventY = 240},[ "F,0,241732,242550,1" ])
              ]

samples ∷ Text
samples = Data.Text.concat
  [ "//Storyboard Sound Samples\n"
  , "Sample,63450,2,\"pass.mp3\",100\n"
  , "Sample,115244,2,\"pass.mp3\",100\n"
  , "Sample,196480,2,\"pass.mp3\",100\n"
  , "Sample,63450,1,\"fail.mp3\",100\n"
  , "Sample,115244,1,\"fail.mp3\",100\n"
  , "Sample,196480,1,\"fail.mp3\",100\n"
  ]

commands ∷ Text
commands = Data.Text.concat
  [ " F,0,108361,,1\n"
  , " M,0,108361,108906,680,326,540,224\n"
  , " R,0,108361,108906,0,0.3891199\n"
  , " M,0,108906,109247,540,224,479,128\n"
  , " M,0,109247,109520,479,128,439,65\n"
  , " R,0,108906,109792,0.3891199,0\n"
  , " M,0,109520,109792,439,65,389,111\n"
  , " M,0,109792,110883,389,111,144,-71\n"
  ]

aniCommands ∷ Text
aniCommands = Data.Text.concat
  [ "Animation,Pass,Centre,\"Captured\\S3-.jpg\",320,240,4,150,LoopForever\n"
  , " M,0,128329,,320,240\n"
  , " F,0,128329,182850,1\n"
  ]

storyboardSample ∷ Text
storyboardSample = Data.Text.concat
  [ "//Background and Video events\n"
  , "//Storyboard Layer 0 (Background)\n"
  , "Sprite,Background,Centre,\"bg.png\",320,240\n"
  , " F,0,0,,0\n"
  , "Sprite,Background,TopLeft,\"Captured\\video.mp4_000010266.jpg\",0,480\n"
  , " M,0,5590,7498,0,480,0,-480\n"
  , " F,0,5590,7498,1\n"
  , " M,0,7498,,0,-480\n"
  , " F,0,7498,8724,1\n"
  , "Sprite,Background,Centre,\"Captured\\white.png\",320,240\n"
  , " F,0,61405,61678,1\n"
  , " F,0,61678,63450,1\n"
  , "Sprite,Background,Centre,\"Captured\\white.png\",320,240\n"
  , " F,0,112791,115244,1\n"
  , "Sprite,Background,Centre,\"Captured\\white.png\",320,240\n"
  , " F,0,5317,5590,0,1\n"
  , " M,0,5590,,320,240\n"
  , " F,0,5590,6612,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000011733.jpg\",320,240\n"
  , " F,0,8724,10292,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000012333.jpg\",320,240\n"
  , " F,0,10292,11110,1\n"
  , " F,0,11110,11655,1\n"
  , " F,0,11655,12200,1\n"
  , " F,0,12200,13291,1\n"
  , " F,0,13291,14449,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000012966.jpg\",320,240\n"
  , " F,0,11110,11655,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000012966.jpg\",320,240\n"
  , " F,0,12200,12745,1\n"
  , "Sprite,Background,Centre,\"Captured\\white.png\",320,240\n"
  , " F,0,14449,15131,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000015433.jpg\",320,240\n"
  , " F,0,15131,15744,1\n"
  , " F,0,15744,16153,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000016433.jpg\",320,240\n"
  , " F,0,16153,17175,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000017233.jpg\",320,240\n"
  , " F,0,17175,18197,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000017666.jpg\",320,240\n"
  , " F,0,18197,18606,1\n"
  , " F,0,18606,18811,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000018033.jpg\",320,240\n"
  , " F,0,18811,19356,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000018333.jpg\",320,240\n"
  , " F,0,19356,19697,1\n"
  , "Sprite,Background,TopLeft,\"Captured\\video.mp4_000018933.jpg\",-640,0\n"
  , " F,0,19697,20242,1\n"
  , " M,0,20242,21469,-640,0,0,0\n"
  , " F,0,20242,21537,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000021200.jpg\",320,240\n"
  , " F,0,21537,21673,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000021700.jpg\",320,240\n"
  , " F,0,21673,22355,1\n"
  , " F,0,22355,23377,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000023733.jpg\",320,240\n"
  , " F,0,23377,29102,1\n"
  , "Sprite,Background,Centre,\"Captured\\Blackbg.png\",320,240\n"
  , " F,0,24740,29102,0.4163199\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000030333.jpg\",320,240\n"
  , " M,0,28556,,320,240\n"
  , " F,0,28556,29102,0,1\n"
  , " F,0,29102,32373,1\n"
  , "Sprite,Background,TopLeft,\"Captured\\video.mp4_000050766.jpg\",320,240\n"
  , " M,0,49819,50365,-640,0,0,0\n"
  , " F,0,50365,50433,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000050933.jpg\",320,240\n"
  , " F,0,50433,50569,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000051633.jpg\",320,240\n"
  , " F,0,50569,50842,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000051633.jpg\",320,240\n"
  , " F,0,50569,50842,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000051933.jpg\",320,240\n"
  , " F,0,50842,51114,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000052266.jpg\",320,240\n"
  , " F,0,51114,51319,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000052433.jpg\",320,240\n"
  , " F,0,51319,51591,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000052266.jpg\",320,240\n"
  , " F,0,51591,52137,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000053100.jpg\",320,240\n"
  , " M,0,52137,,320,240\n"
  , " F,0,52137,55067,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000055933.jpg\",320,240\n"
  , " F,0,55067,58883,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000059500.jpg\",320,240\n"
  , " F,0,58883,59224,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000059666.jpg\",320,240\n"
  , " F,0,59224,59497,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000060000.jpg\",320,240\n"
  , " F,0,59497,59769,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000060166.jpg\",320,240\n"
  , " F,0,59769,60042,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000060433.jpg\",320,240\n"
  , " F,0,60042,60315,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000060700.jpg\",320,240\n"
  , " F,0,60315,60587,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000060433.jpg\",320,240\n"
  , " F,0,60587,60860,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000060700.jpg\",320,240\n"
  , " F,0,60860,61132,1\n"
  , " F,0,61132,,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000060433.jpg\",320,240\n"
  , " F,0,61132,61405,1\n"
  , " F,0,61405,,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000030333.jpg\",320,240\n"
  , " F,0,64540,65630,0,1\n"
  , " F,0,65630,68902,1\n"
  , " M,0,68902,,320,240\n"
  , " F,0,68902,69447,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000068333.jpg\",320,240\n"
  , " F,0,63450,64540,1\n"
  , " F,0,64540,65085,1,0\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000086566.jpg\",320,240\n"
  , " F,0,104340,106521,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000088466.jpg\",320,240\n"
  , " M,0,106521,106794,320,240\n"
  , " F,0,106521,106794,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000088633.jpg\",320,240\n"
  , " F,0,106794,107134,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000089966.jpg\",320,240\n"
  , " F,0,107134,107611,1\n"
  , " F,0,107611,108157,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000090900.jpg\",320,240\n"
  , " F,0,108157,109792,1\n"
  , " F,0,109792,110883,1\n"
  , " M,0,110883,,320,240\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000104200.jpg\",320,240\n"
  , " R,0,110883,,0\n"
  , " F,0,110883,111837,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000104833.jpg\",320,240\n"
  , " F,0,111837,112518,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000106066.jpg\",320,240\n"
  , " F,0,112518,112791,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000108400.jpg\",320,240\n"
  , " F,0,115244,117425,1\n"
  , " F,0,117425,,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000110933.jpg\",320,240\n"
  , " F,0,117425,118515,1\n"
  , " F,0,118515,119061,1,0\n"
  , " F,0,119061,,0\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000115433.jpg\",320,240\n"
  , " F,0,118515,119061,0,1\n"
  , " F,0,119061,119606,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000118200.jpg\",320,240\n"
  , " F,0,119606,119878,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000118633.jpg\",320,240\n"
  , " F,0,119878,120151,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000119300.jpg\",320,240\n"
  , " F,0,120151,120424,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000120233.jpg\",320,240\n"
  , " F,0,120424,120696,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000120466.jpg\",320,240\n"
  , " F,0,120696,120969,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000120800.jpg\",320,240\n"
  , " F,0,120969,121242,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000120966.jpg\",320,240\n"
  , " F,0,121242,121514,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000121100.jpg\",320,240\n"
  , " F,0,121514,121787,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000121233.jpg\",320,240\n"
  , " F,0,121787,122059,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000121433.jpg\",320,240\n"
  , " F,0,122059,122605,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000123266.jpg\",320,240\n"
  , " F,0,122605,123968,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000030333.jpg\",320,240\n"
  , " F,0,123695,123968,0,1\n"
  , " F,0,123968,128329,1,0.992674\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000181300.jpg\",320,240\n"
  , " M,0,182850,183395,320,240\n"
  , " F,0,182850,183395,1\n"
  , " F,0,183395,183668,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000182700.jpg\",320,240\n"
  , " F,0,183668,184485,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000183766.jpg\",320,240\n"
  , " F,0,184485,185031,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000184333.jpg\",320,240\n"
  , " F,0,185031,185849,1\n"
  , "Sprite,Background,Centre,\"Captured\\white.png\",320,240\n"
  , " F,0,185849,186462,1\n"
  , " F,0,186462,187212,1,0\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000185466.jpg\",320,240\n"
  , " F,0,186462,186939,0,0.636\n"
  , " F,0,186939,187212,0.636,1\n"
  , "Sprite,Background,Centre,\"Captured\\white.png\",320,240\n"
  , " F,0,187212,187757,1,0\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000187200.jpg\",320,240\n"
  , " F,0,187552,,0\n"
  , " F,0,187552,187893,0,1\n"
  , " F,0,187893,188847,1\n"
  , "Sprite,Background,Centre,\"Captured\\white.png\",320,240\n"
  , " F,0,188847,189392,1,0\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000190800.jpg\",320,240\n"
  , " F,0,188847,189392,0,1\n"
  , " F,0,189392,191573,1\n"
  , " F,0,191573,191709,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000193033.jpg\",320,240\n"
  , " F,0,191709,193550,1\n"
  , " F,0,193550,193686,1,0.3333333\n"
  , " F,0,193686,193754,0.3333333,0\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000196000.jpg\",320,240\n"
  , " F,0,195526,200024,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000200366.jpg\",320,240\n"
  , " F,0,200024,200569,0,1\n"
  , " F,0,200569,201114,1\n"
  , " F,0,201114,201659,1\n"
  , "Sprite,Background,Centre,\"Captured\\white.png\",320,240\n"
  , " F,0,201659,202205,1,0\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000202500.jpg\",320,240\n"
  , " F,0,201659,202205,0,1\n"
  , " F,0,202205,203568,1\n"
  , "Sprite,Background,Centre,\"Captured\\white.png\",320,240\n"
  , " F,0,203568,204113,1,0\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000204833.jpg\",320,240\n"
  , " F,0,203568,205612,1\n"
  , "Sprite,Background,Centre,\"Captured\\white.png\",320,240\n"
  , " F,0,205612,206157,1,0\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000206733.jpg\",320,240\n"
  , " F,0,205612,207793,1\n"
  , " F,0,207793,207929,1,0\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000208566.jpg\",320,240\n"
  , " F,0,207793,207929,0,1\n"
  , " F,0,207929,208066,1\n"
  , " F,0,208066,209020,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000209933.jpg\",320,240\n"
  , " F,0,209020,209565,1\n"
  , " F,0,209565,210110,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000210233.jpg\",320,240\n"
  , " F,0,210110,210928,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000211666.jpg\",320,240\n"
  , " F,0,210928,211473,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000212266.jpg\",320,240\n"
  , " M,0,211473,,320,240\n"
  , " F,0,211473,213109,1\n"
  , "Sprite,Background,Centre,\"Captured\\white.png\",320,240\n"
  , " F,0,213109,213381,1\n"
  , " F,0,213381,213654,1,0\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000213700.jpg\",320,240\n"
  , " F,0,213381,213654,0,1\n"
  , " F,0,213654,215562,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000216766.jpg\",320,240\n"
  , " F,0,215562,217470,1\n"
  , "Sprite,Background,Centre,\"Captured\\video.mp4_000217633.jpg\",320,240\n"
  , " F,0,217470,218288,1,0\n"
  , "Sprite,Background,Centre,\"Captured\\white.png\",320,240\n"
  , " F,0,200024,200569,1,0\n"
  , "//Storyboard Layer 1 (Fail)\n"
  , "Animation,Fail,Centre,\"Captured\\NS-.jpg\",320,240,4,300,LoopForever\n"
  , " F,0,32373,49819,1\n"
  , "Animation,Fail,Centre,\"Captured\\NS-.jpg\",320,240,4,300,LoopForever\n"
  , " F,0,69447,104340,1\n"
  , "Animation,Fail,Centre,\"Captured\\NS-.jpg\",320,240,4,300,LoopForever\n"
  , " F,0,128329,182850,1\n"
  , "Sprite,Fail,Centre,\"Captured\\SmallChen1.png\",320,240\n"
  , " F,0,108361,,1\n"
  , " M,0,108361,108906,680,326,540,224\n"
  , " R,0,108361,108906,0,0.3891199\n"
  , " M,0,108906,109247,540,224,479,128\n"
  , " R,0,108906,109520,0.3891199\n"
  , " M,0,109247,109520,479,128,439,65\n"
  , " M,0,109520,,439,65\n"
  , " M,0,109520,110883,439,65,723,252\n"
  , " R,0,109520,110883,0.3891199,14.15168\n"
  , "Sprite,Fail,Centre,\"Captured\\ChenDiagonal.png\",320,240\n"
  , " M,0,113881,114971,187,-131,741,272\n"
  , " R,0,113881,114971,0,7.82336\n"
  , " M,0,114971,114972,741,272,758,293\n"
  , "Animation,Fail,Centre,\"Captured\\NS-.jpg\",320,240,4,300,LoopForever\n"
  , " F,0,217743,218288,0,1\n"
  , " F,0,218288,241732,1\n"
  , "Sprite,Fail,Centre,\"Captured\\video.mp4_000245466.jpg\",320,240\n"
  , " F,0,251001,251273,0,1\n"
  , " F,0,251273,252636,1\n"
  , " F,0,252636,256998,1\n"
  , " F,0,256998,259179,1\n"
  , "Sprite,Fail,Centre,\"Captured\\MISSIONFAILEO.jpg\",320,240\n"
  , " F,0,193550,193754,0,1\n"
  , " F,0,193754,195526,1\n"
  , "Sprite,Fail,Centre,\"fail.png\",320,240\n"
  , " F,0,63450,63586,1,0\n"
  , " F,0,63586,63722,0,1\n"
  , " F,0,63722,65085,1\n"
  , " F,0,65085,65222,1,0\n"
  , "Sprite,Fail,Centre,\"fail.png\",320,240\n"
  , " F,0,115244,115381,1,0\n"
  , " F,0,115381,115517,0,1\n"
  , " F,0,115517,116880,1\n"
  , " F,0,116880,117425,1\n"
  , " F,0,117425,,1\n"
  , "Sprite,Fail,Centre,\"fail.png\",320,240\n"
  , " F,0,196480,196548,1,0\n"
  , " F,0,196548,196616,0,1\n"
  , " F,0,196616,198116,1\n"
  , " F,0,198116,198252,1,0\n"
  , "//Storyboard Layer 2 (Pass)\n"
  , "Sprite,Pass,Centre,\"Captured\\video.mp4_000194100.jpg\",320,240\n"
  , " F,0,193550,193754,0,1\n"
  , " F,0,193754,195526,1\n"
  , "Animation,Pass,Centre,\"Captured\\MS.jpg\",320,240,4,150,LoopForever\n"
  , " F,0,32373,49819,1\n"
  , "Sprite,Pass,Centre,\"Captured\\ChenDiagonal.png\",320,240\n"
  , " F,0,60575,60724,0.86176,1\n"
  , " M,0,60724,62359,737,309,189,-106\n"
  , "Animation,Pass,Centre,\"Captured\\BS-.jpg\",320,240,4,150,LoopForever\n"
  , " M,0,69447,,320,240\n"
  , " F,0,69447,104340,1\n"
  , "Animation,Pass,Centre,\"Captured\\S3-.jpg\",320,240,4,150,LoopForever\n"
  , " M,0,128329,,320,240\n"
  , " F,0,128329,182850,1\n"
  , "Sprite,Pass,Centre,\"Captured\\SmallChen1.png\",320,240\n"
  , " F,0,108361,,1\n"
  , " M,0,108361,108906,680,326,540,224\n"
  , " R,0,108361,108906,0,0.3891199\n"
  , " M,0,108906,109247,540,224,479,128\n"
  , " M,0,109247,109520,479,128,439,65\n"
  , " R,0,108906,109792,0.3891199,0\n"
  , " M,0,109520,109792,439,65,389,111\n"
  , " M,0,109792,110883,389,111,144,-71\n"
  , "Animation,Pass,Centre,\"Captured\\ARGH.jpg\",320,240,4,100,LoopForever\n"
  , " F,0,217743,218288,0,1\n"
  , " F,0,218288,241732,1\n"
  , "Animation,Pass,Centre,\"Captured\\YuyuYoumu.png\",320,240,2,500,LoopForever\n"
  , " F,0,217743,218288,0,0.5\n"
  , " M,0,218288,,544,59\n"
  , " S,0,218288,,0.7952\n"
  , " F,0,218288,241732,0.5\n"
  , "Animation,Pass,Centre,\"Captured\\MystAli.png\",320,240,2,500,LoopForever\n"
  , " M,0,217743,,124,66\n"
  , " S,0,217743,,0.8259199\n"
  , " F,0,217743,218288,0,0.5\n"
  , " F,0,218288,241732,0.5\n"
  , "Animation,Pass,Centre,\"Captured\\RiguRumi.png\",320,240,2,500,LoopForever\n"
  , " M,0,217743,,115,384\n"
  , " F,0,217743,218288,0,0.5\n"
  , " F,0,218288,241732,0.5\n"
  , "Animation,Pass,Centre,\"Captured\\Mari.png\",320,240,2,500,LoopForever\n"
  , " M,0,217743,,537,385\n"
  , " F,0,217743,218288,0,0.5\n"
  , " F,0,218288,241732,0.5\n"
  , "Sprite,Pass,Centre,\"Captured\\video.mp4_000248266.jpg\",320,240\n"
  , " F,0,251001,251273,0,1\n"
  , " F,0,251273,252636,1\n"
  , " F,0,252636,259179,1\n"
  , "Sprite,Pass,Centre,\"pass.png\",320,240\n"
  , " F,0,63450,63586,1,0\n"
  , " F,0,63586,63722,0,1\n"
  , " F,0,63722,65085,1\n"
  , " F,0,65085,65222,1,0\n"
  , "Sprite,Pass,Centre,\"pass.png\",320,240\n"
  , " F,0,115244,115381,1,0\n"
  , " F,0,115381,115517,0,1\n"
  , " F,0,115517,116880,1\n"
  , " F,0,116880,117425,1\n"
  , " F,0,117425,,1\n"
  , "Sprite,Pass,Centre,\"pass.png\",320,240\n"
  , " F,0,196480,196548,1,0\n"
  , " F,0,196548,196616,0,1\n"
  , " F,0,196616,198116,1\n"
  , " F,0,198116,198252,1,0\n"
  , "//Storyboard Layer 3 (Foreground)\n"
  , "Sprite,Foreground,Centre,\"Captured\\video.mp4_000107700.png\",320,240\n"
  , " F,0,112791,113063,1\n"
  , "Sprite,Foreground,Centre,\"Captured\\video.mp4_000107866.png\",320,240\n"
  , " F,0,113063,113336,1\n"
  , "Sprite,Foreground,Centre,\"Captured\\video.mp4_000108000.png\",320,240\n"
  , " F,0,113336,113609,1\n"
  , "Sprite,Foreground,Centre,\"Captured\\video.mp4_000108133.png\",320,240\n"
  , " F,0,113609,113881,1\n"
  , "Sprite,Foreground,Centre,\"Captured\\video.mp4_000108266.png\",320,240\n"
  , " F,0,113881,114154,1\n"
  , " F,0,114154,115244,1\n"
  , "Sprite,Foreground,Centre,\"Captured\\video.mp4_000060700.png\",320,240\n"
  , " F,0,61405,61678,1\n"
  , " F,0,61678,63450,1\n"
  , "Sprite,Foreground,TopLeft,\"Captured\\BikeSide.png\",320,240\n"
  , " F,0,30737,,0.77472\n"
  , " M,0,30737,32237,640,0,-640,0\n"
  , "Sprite,Foreground,TopLeft,\"Captured\\BikeSide.png\",320,240\n"
  , " F,0,67266,,1\n"
  , " M,0,67266,68902,640,0,-640,0\n"
  , "Sprite,Foreground,Centre,\"Captured\\YEEEEEEEEEEEAH.png\",320,240\n"
  , " M,0,24740,,322,262\n"
  , " F,0,24740,25285,0,1\n"
  , " F,0,25285,28556,1\n"
  , " F,0,28556,29102,1,0\n"
  , "Sprite,Foreground,Centre,\"Captured\\ChenDiagonal.png\",320,240\n"
  , " F,0,104613,,1\n"
  , " M,0,104613,105976,685,466,-38,-78\n"
  , "Sprite,Foreground,TopLeft,\"Captured\\BikeSide.png\",320,240\n"
  , " F,0,126966,,1\n"
  , " M,0,126966,128057,640,0,-640,0\n"
  , "Sprite,Foreground,Centre,\"Captured\\video.mp4_000229333.jpg\",320,240\n"
  , " F,0,242550,243368,1\n"
  , "Sprite,Foreground,Centre,\"Captured\\video.mp4_000229800.jpg\",320,240\n"
  , " F,0,243368,243709,1\n"
  , "Sprite,Foreground,Centre,\"Captured\\video.mp4_000231333.jpg\",320,240\n"
  , " F,0,243709,244458,1\n"
  , "Sprite,Foreground,Centre,\"Captured\\video.mp4_000233466.jpg\",320,240\n"
  , " F,0,244458,245003,1\n"
  , "Sprite,Foreground,Centre,\"Captured\\video.mp4_000235233.jpg\",320,240\n"
  , " F,0,245003,245549,1\n"
  , "Sprite,Foreground,Centre,\"Captured\\video.mp4_000236400.jpg\",320,240\n"
  , " F,0,245549,246094,1\n"
  , "Sprite,Foreground,Centre,\"Captured\\white.png\",320,240\n"
  , " F,0,247184,247457,1,0\n"
  , "Sprite,Foreground,Centre,\"Captured\\white.png\",320,240\n"
  , " F,0,248275,248547,1,0\n"
  , "Sprite,Foreground,Centre,\"Captured\\video.mp4_000241233.jpg\",320,240\n"
  , " F,0,248275,248547,0,1\n"
  , " F,0,248547,249365,1\n"
  , "Sprite,Foreground,Centre,\"Captured\\white.png\",320,240\n"
  , " F,0,249365,249638,1,0\n"
  , "Sprite,Foreground,Centre,\"Captured\\video.mp4_000237266.jpg\",320,240\n"
  , " F,0,246094,246639,1\n"
  , " F,0,246639,246980,1\n"
  , " F,0,246980,247184,1\n"
  , "Sprite,Foreground,Centre,\"Captured\\video.mp4_000239600.jpg\",320,240\n"
  , " F,0,247184,247457,0,1\n"
  , " F,0,247457,248275,1\n"
  , "Sprite,Foreground,Centre,\"Captured\\video.mp4_000242300.jpg\",320,240\n"
  , " F,0,249365,249638,0,1\n"
  , " F,0,249638,249910,1\n"
  , "Sprite,Foreground,Centre,\"Captured\\white.png\",320,240\n"
  , " F,0,249910,250183,1,0\n"
  , "Sprite,Foreground,Centre,\"Captured\\video.mp4_000243400.jpg\",320,240\n"
  , " F,0,249910,250183,0,1\n"
  , " F,0,250183,250728,1\n"
  , "Sprite,Foreground,Centre,\"Captured\\white.png\",320,240\n"
  , " F,0,250728,251001,1\n"
  , " F,0,251001,251273,1,0\n"
  , "Sprite,Foreground,Centre,\"Captured\\video.mp4_000225333.jpg\",320,240\n"
  , " F,0,241732,242550,1\n"
  , "//Storyboard Sound Samples\n"
  ]
