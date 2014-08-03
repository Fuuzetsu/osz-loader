{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Osu.OszLoader.OsuParser.HitObjectsSpec (spec) where

import Data.Attoparsec.Text
import Data.Text
import Game.Osu.OszLoader.OsuParser.HitObjects
import Game.Osu.OszLoader.Types
import Test.Hspec

spec ∷ Spec
spec = do
  describe "HitObjects section" $ do

    context "subparsers" $ do
      it "spinnerP" $ do
        parseOnly spinnerP "256,192,6157,1,0"
          `shouldBe` Right (256,192,6157,1,0)

      it "sliderP" $ do
        parseOnly sliderP "64,176,7400,2,0,B|-32:264|72:360,1,180\n"
          `shouldBe` Right (64,176,7400,2,0,"B|-32:264|72:360,1,180")

      it "circleP" $ do
        parseOnly circleP "164,260,2434,1,0,0:0:0:0:"
          `shouldBe` Right (164,260,2434,1,0,(0,0,0,0))

    it "can parse the provided sample" $ do
      parseOnly hitObjectsSection hitObjectsSample `shouldBe`
        Right [ Spinner (256,192,6157,1,0)
              , Spinner (256,192,6334,1,0)
              , Spinner (256,192,6512,1,0)
              , Spinner (112,88,6867,5,4)
              , Spinner (184,136,7044,1,0)
              , Spinner (144,216,7222,1,8)
              , Slider (64,176,7400,2,0,"B|-32:264|72:360,1,180")
              , Spinner (47,334,7932,1,8)
              , Slider (112,264,8110,2,0,"B|208:176|104:80,1,180")
              , Spinner (64,176,8642,1,10)
              , Spinner (112,264,8820,1,0)
              , Slider (192,304,8997,6,0,"B|240:296|264:240,1,90,2|0")
              , Spinner (152,128,9352,1,10)
              , Spinner (32,240,9530,1,0)
              , Slider (192,304,9707,6,0,"B|272:384|376:312,1,180,0|8")
              , Slider (440,280,10240,2,0,"B|360:200|256:272,1,180")
              , Spinner (192,304,10772,1,8)
              , Slider (128,240,10950,2,0,"B|128:56,1,180")
              , Spinner (208,96,11482,1,10)
              , Spinner (280,56,11660,1,0)
              , Spinner (208,16,11837,1,2)
              , Spinner (136,56,12015,1,8)
              , Spinner (208,96,12192,5,10)
              , Slider (128,136,12370,2,0,"B|216:216|304:120,1,180,4|0")
              , Slider (368,96,12902,2,0,"B|280:16|192:112,1,180,8|0")
              , Spinner (160,168,13435,1,0)
              , Spinner (208,248,13613,1,8)
              , Slider (296,216,13790,2,0,"B|488:216,1,180")
              , Slider (432,296,14323,2,0,"B|240:296,1,180,8|0")
              , Spinner (296,216,14855,1,0)
              , Spinner (248,136,15033,1,8)
              , Spinner (168,168,15210,1,0)
              , Spinner (208,248,15388,5,2)
              , Slider (296,216,15565,2,0,"B|352:168|320:96,1,90,0|10")
              , Spinner (248,136,15920,1,0)
              , Slider (168,168,16098,2,0,"B|120:192|120:256,1,90,2|0")
              , Spinner (208,248,16453,1,10)
              , Slider (288,280,16630,2,0,"B|376:352|464:256,1,180,4|0")
              , Spinner (449,271,17163,5,10)
              , Spinner (336,152,17340,1,0)
              , Spinner (200,248,17518,1,2)
              , Spinner (312,368,17695,1,8)
              , Spinner (368,296,17873,5,10)
              , Spinner (280,288,18050,1,0)
              , Spinner (312,368,18228,1,4)
              , Spinner (96,80,29589,5,4)
              , Spinner (40,152,29766,1,0)
              , Slider (120,192,29944,2,0,"B|200:264|320:216,1,180,0|0")
              , Spinner (344,160,30476,1,0)
              , Slider (280,96,30654,2,0,"B|232:64|168:112,1,90")
              , Spinner (248,168,31009,1,0)
              , Slider (344,160,31187,2,0,"B|416:232|352:328,1,180")
              , Spinner (272,280,31719,5,0)
              , Spinner (192,320,31897,1,0)
              , Slider (128,256,32074,2,0,"B|72:296|104:360,1,90")
              , Spinner (192,320,32429,1,4)
              , Spinner (128,256,32607,1,0)
              , Slider (200,208,32784,2,0,"B|240:168|208:104,1,90")
              , Spinner (128,144,33139,1,0)
              , Spinner (112,232,33317,1,0)
              , Spinner (176,296,33494,5,0)
              , Spinner (264,312,33672,1,0)
              , Slider (344,272,33849,2,0,"B|384:224|352:168,2,90")
              , Slider (280,208,34382,2,0,"B|240:96|352:40,1,180")
              , Slider (376,128,34914,2,0,"B|424:112|448:56,1,90")
              , Slider (448,176,35269,6,0,"B|448:360,1,180,4|8")
              , Slider (368,320,35802,2,0,"B|368:136,1,180")
              , Spinner (448,96,36334,1,8)
              , Spinner (368,48,36512,1,0)
              , Slider (288,96,36689,2,0,"B|288:184,1,90")
              , Slider (224,256,37044,2,0,"B|136:312|48:248,2,180,8|0|8")
              , Slider (368,136,38110,6,0,"B|456:216|360:312,1,180,0|8")
              , Slider (296,336,38642,2,0,"B|208:256|304:160,1,180")
              , Spinner (368,136,39175,1,8)
              , Spinner (288,96,39352,1,0)
              , Spinner (208,136,39530,5,2)
              , Spinner (144,280,39707,1,0)
              , Spinner (296,344,39885,1,10)
              , Spinner (376,200,40062,1,0)
              , Spinner (288,168,40240,5,2)
              , Spinner (248,248,40417,1,8)
              , Spinner (328,280,40595,1,10)
              , Spinner (376,200,40772,1,0)
              , Slider (248,248,40950,6,0,"B|144:248,2,90,4|0|0")
              , Spinner (288,168,41482,1,0)
              , Spinner (376,200,41660,1,0)
              , Spinner (328,280,41837,1,0)
              , Slider (248,248,42015,6,0,"B|168:152|288:72,1,180")
              , Slider (312,168,42547,2,0,"B|392:264|272:344,1,180")
              , Spinner (240,256,43080,5,0)
              , Spinner (296,184,43258,1,0)
              , Spinner (368,240,43435,1,0)
              , Slider (304,304,43613,2,0,"B|328:352|400:360,1,90,0|4")
              , Spinner (408,280,43968,5,0)
              , Slider (456,208,44145,2,0,"B|488:152|456:104,1,90")
              , Slider (392,72,44500,2,0,"B|352:112|400:176,2,90")
              , Spinner (465,121,45033,1,0)
              , Spinner (386,154,45210,5,0)
              , Slider (304,192,45388,2,0,"B|256:160|272:96,1,90")
              , Slider (200,56,45743,2,0,"B|112:120|160:248,1,180")
              , Spinner (149,216,46275,1,0)
              , Slider (232,176,46453,2,0,"B|288:272|248:352,1,180,4|0")
              , Spinner (168,304,46985,5,8)
              , Spinner (250,347,47163,1,0)
              , Spinner (336,320,47340,1,0)
              , Slider (416,288,47518,2,0,"B|456:248|448:192,1,90,0|8")
              , Slider (352,224,47873,2,0,"B|288:136|336:40,1,180")
              , Slider (240,72,48405,2,0,"B|136:88,1,90,8|0")
              , Slider (88,152,48760,2,0,"B|192:184,1,90")
              , Slider (240,240,49115,2,0,"B|144:272,1,90,8|0")
              , Slider (72,304,49471,6,0,"B|144:384|272:344,1,180,4|8")
              , Spinner (236,353,50003,1,0)
              , Slider (320,312,50181,2,0,"B|248:232|120:272,1,180,0|8")
              , Spinner (155,262,50713,5,0)
              , Spinner (112,104,50891,1,2)
              , Spinner (272,72,51068,1,0)
              , Spinner (320,224,51246,1,10)
              , Spinner (232,200,51423,5,0)
              , Spinner (288,128,51601,1,2)
              , Spinner (376,152,51778,1,8)
              , Spinner (320,224,51956,1,10)
              , Spinner (288,128,52133,1,0)
              , Slider (320,224,52311,6,0,"B|256:320|136:248,1,180,4|8")
              , Slider (72,240,52843,2,0,"B|136:144|256:216,1,180")
              , Spinner (320,224,53376,1,8)
              , Slider (400,184,53553,2,0,"B|400:-8,1,180")
              , Spinner (400,4,54086,5,8)
              , Spinner (464,152,54263,1,0)
              , Spinner (320,224,54441,1,0)
              , Spinner (248,80,54618,1,0)
              , Spinner (328,40,54796,5,8)
              , Spinner (320,128,54973,1,0)
              , Slider (248,80,55151,2,0,"B|48:80,2,180,4|8|0")
              , Spinner (320,128,56039,1,0)
              , Spinner (328,40,56216,1,8)
              , Slider (400,96,56394,2,0,"B|480:168|400:264,1,180")
              , Spinner (336,312,56926,1,8)
              , Spinner (248,336,57104,1,0)
              , Slider (160,344,57281,2,0,"B|112:360|48:296,2,90,0|0|8")
              , Slider (208,264,57814,6,0,"B|312:240|336:144,1,180,4|0")
              , Slider (248,144,58346,2,0,"B|200:112|208:56,1,90,8|0")
              , Slider (136,128,58701,2,0,"B|136:232,2,90,0|0|8")
              , Spinner (206,68,59234,5,0)
              , Spinner (232,152,59411,1,0)
              , Spinner (144,128,59589,1,0)
              , Spinner (206,68,59766,5,10)
              , Spinner (184,152,59944,1,0)
              , Slider (168,240,60121,2,0,"B|152:288|160:336,2,90,2|8|10")
              , Spinner (184,152,60654,1,0)
              , Slider (256,200,60831,6,0,"B|328:120|264:24,1,180,4|8")
              , Slider (184,152,61364,2,0,"B|112:232|184:328,1,180")
              , Spinner (175,316,61897,5,8)
              , Slider (264,288,62074,2,0,"B|336:232|440:248,1,180")
              , Slider (488,176,62607,2,0,"B|440:152|384:160,1,90,8|0")
              , Spinner (256,192,62962,5,0)
              , Spinner (256,192,63139,1,2)
              , Spinner (256,192,63317,1,2)
              , Slider (144,128,63672,6,0,"B|112:16,2,90,4|0|8")
              , Slider (72,176,64204,2,0,"B|160:256|240:144,1,180")
              , Spinner (312,192,64737,1,8)
              , Slider (400,224,64914,2,0,"B|312:160|232:264,1,180")
              , Slider (184,320,65447,6,0,"B|128:336|88:272,1,90,10|0")
              , Slider (56,216,65802,2,0,"B|24:168|72:104,2,90,2|0|10")
              , Spinner (103,293,66334,1,0)
              , Slider (176,232,66512,2,0,"B|256:152|360:232,1,180,0|8")
              , Slider (416,264,67044,2,0,"B|336:344|232:264,1,180")
              , Spinner (176,232,67577,1,8)
              , Slider (128,152,67755,2,0,"B|40:232|120:320,1,180")
              , Slider (192,352,68287,2,0,"B|240:384|288:360,1,90,10|0")
              , Slider (352,320,68642,2,0,"B|400:304|448:320,2,90,2|8|10")
              , Slider (280,264,69175,6,0,"B|184:184|288:88,1,180,4|0")
              , Slider (328,40,69707,2,0,"B|424:120|320:216,1,180,8|0")
              , Slider (264,248,70240,2,0,"B|224:280|152:248,1,90,0|8")
              , Slider (216,176,70595,6,0,"B|136:112|48:176,1,180")
              , Slider (96,248,71127,2,0,"B|176:312|264:248,1,180,8|0")
              , Slider (344,288,71660,2,0,"B|344:376,2,90,0|8|0")
              , Spinner (344,288,72192,5,2)
              , Spinner (240,192,72370,1,0)
              , Spinner (344,88,72547,1,10)
              , Spinner (448,192,72725,1,0)
              , Spinner (344,256,72902,5,2)
              , Spinner (280,192,73080,1,0)
              , Spinner (344,128,73258,1,10)
              , Spinner (408,192,73435,1,0)
              , Spinner (344,192,73613,5,2)
              , Spinner (344,192,73968,1,10)
              , Spinner (344,192,74323,1,2)
              , Spinner (208,280,74500,1,8)
              , Spinner (112,152,74678,1,10)
              , Spinner (248,56,74855,1,0)
              , Spinner (344,192,75033,5,4)
              , Spinner (256,208,75210,1,0)
              , Spinner (296,128,75388,1,8)
              , Slider (344,192,75565,2,0,"B|416:272|328:360,1,180")
              , Spinner (248,352,76098,1,8)
              , Slider (160,352,76275,2,0,"B|88:272|176:184,1,180")
              , Slider (104,128,76808,6,0,"B|96:64|16:40,1,90,10|0")
              , Spinner (16,136,77163,1,2)
              , Slider (104,128,77340,2,0,"B|152:160|216:104,1,90,0|10")
              , Spinner (144,48,77695,1,0)
              , Slider (104,128,77873,2,0,"B|16:216|104:320,1,180,0|8")
              , Slider (128,368,78405,2,0,"B|216:280|128:176,1,180")
              , Spinner (216,144,78938,1,8)
              , Slider (296,96,79115,2,0,"B|496:96,1,180")
              , Spinner (476,96,79648,5,10)
              , Spinner (328,24,79826,1,0)
              , Spinner (264,176,80003,1,2)
              , Spinner (408,248,80181,1,8)
              , Spinner (440,168,80358,5,10)
              , Slider (408,248,80536,2,0,"B|328:336|216:248,1,180,4|0")
              , Slider (168,224,81068,2,0,"B|248:136|360:224,1,180,8|0")
              , Slider (392,136,81601,2,0,"B|416:40,1,90,0|8")
              , Slider (472,112,81956,6,0,"B|496:320,1,180,4|0")
              , Slider (416,336,82488,2,0,"B|392:128,1,180,8|0")
              , Slider (312,120,83021,2,0,"B|312:224,2,90,0|8|0")
              , Spinner (312,120,83553,5,2)
              , Spinner (168,48,83731,1,0)
              , Spinner (88,184,83908,1,10)
              , Spinner (232,264,84086,1,0)
              , Spinner (240,176,84263,5,2)
              , Spinner (168,128,84441,1,0)
              , Spinner (160,216,84618,1,10)
              , Slider (232,264,84796,2,0,"B|312:352|400:256,1,180,4|0")
              , Spinner (387,268,85329,1,8)
              , Slider (312,224,85506,6,0,"B|232:136|144:232,1,180,0|8")
              , Slider (88,280,86039,2,0,"B|32:264|24:192,1,90,8|0")
              , Slider (88,152,86394,6,0,"B|168:64|272:104,1,180,4|8")
              , Spinner (328,136,86926,1,0)
              , Spinner (392,200,87104,1,2)
              ]



hitObjectsSample ∷ Text
hitObjectsSample = Data.Text.concat
  [ "[HitObjects]\n"
  , "256,192,6157,1,0\n"
  , "256,192,6334,1,0\n"
  , "256,192,6512,1,0\n"
  , "112,88,6867,5,4\n"
  , "184,136,7044,1,0\n"
  , "144,216,7222,1,8\n"
  , "64,176,7400,2,0,B|-32:264|72:360,1,180\n"
  , "47,334,7932,1,8\n"
  , "112,264,8110,2,0,B|208:176|104:80,1,180\n"
  , "64,176,8642,1,10\n"
  , "112,264,8820,1,0\n"
  , "192,304,8997,6,0,B|240:296|264:240,1,90,2|0\n"
  , "152,128,9352,1,10\n"
  , "32,240,9530,1,0\n"
  , "192,304,9707,6,0,B|272:384|376:312,1,180,0|8\n"
  , "440,280,10240,2,0,B|360:200|256:272,1,180\n"
  , "192,304,10772,1,8\n"
  , "128,240,10950,2,0,B|128:56,1,180\n"
  , "208,96,11482,1,10\n"
  , "280,56,11660,1,0\n"
  , "208,16,11837,1,2\n"
  , "136,56,12015,1,8\n"
  , "208,96,12192,5,10\n"
  , "128,136,12370,2,0,B|216:216|304:120,1,180,4|0\n"
  , "368,96,12902,2,0,B|280:16|192:112,1,180,8|0\n"
  , "160,168,13435,1,0\n"
  , "208,248,13613,1,8\n"
  , "296,216,13790,2,0,B|488:216,1,180\n"
  , "432,296,14323,2,0,B|240:296,1,180,8|0\n"
  , "296,216,14855,1,0\n"
  , "248,136,15033,1,8\n"
  , "168,168,15210,1,0\n"
  , "208,248,15388,5,2\n"
  , "296,216,15565,2,0,B|352:168|320:96,1,90,0|10\n"
  , "248,136,15920,1,0\n"
  , "168,168,16098,2,0,B|120:192|120:256,1,90,2|0\n"
  , "208,248,16453,1,10\n"
  , "288,280,16630,2,0,B|376:352|464:256,1,180,4|0\n"
  , "449,271,17163,5,10\n"
  , "336,152,17340,1,0\n"
  , "200,248,17518,1,2\n"
  , "312,368,17695,1,8\n"
  , "368,296,17873,5,10\n"
  , "280,288,18050,1,0\n"
  , "312,368,18228,1,4\n"
  , "96,80,29589,5,4\n"
  , "40,152,29766,1,0\n"
  , "120,192,29944,2,0,B|200:264|320:216,1,180,0|0\n"
  , "344,160,30476,1,0\n"
  , "280,96,30654,2,0,B|232:64|168:112,1,90\n"
  , "248,168,31009,1,0\n"
  , "344,160,31187,2,0,B|416:232|352:328,1,180\n"
  , "272,280,31719,5,0\n"
  , "192,320,31897,1,0\n"
  , "128,256,32074,2,0,B|72:296|104:360,1,90\n"
  , "192,320,32429,1,4\n"
  , "128,256,32607,1,0\n"
  , "200,208,32784,2,0,B|240:168|208:104,1,90\n"
  , "128,144,33139,1,0\n"
  , "112,232,33317,1,0\n"
  , "176,296,33494,5,0\n"
  , "264,312,33672,1,0\n"
  , "344,272,33849,2,0,B|384:224|352:168,2,90\n"
  , "280,208,34382,2,0,B|240:96|352:40,1,180\n"
  , "376,128,34914,2,0,B|424:112|448:56,1,90\n"
  , "448,176,35269,6,0,B|448:360,1,180,4|8\n"
  , "368,320,35802,2,0,B|368:136,1,180\n"
  , "448,96,36334,1,8\n"
  , "368,48,36512,1,0\n"
  , "288,96,36689,2,0,B|288:184,1,90\n"
  , "224,256,37044,2,0,B|136:312|48:248,2,180,8|0|8\n"
  , "368,136,38110,6,0,B|456:216|360:312,1,180,0|8\n"
  , "296,336,38642,2,0,B|208:256|304:160,1,180\n"
  , "368,136,39175,1,8\n"
  , "288,96,39352,1,0\n"
  , "208,136,39530,5,2\n"
  , "144,280,39707,1,0\n"
  , "296,344,39885,1,10\n"
  , "376,200,40062,1,0\n"
  , "288,168,40240,5,2\n"
  , "248,248,40417,1,8\n"
  , "328,280,40595,1,10\n"
  , "376,200,40772,1,0\n"
  , "248,248,40950,6,0,B|144:248,2,90,4|0|0\n"
  , "288,168,41482,1,0\n"
  , "376,200,41660,1,0\n"
  , "328,280,41837,1,0\n"
  , "248,248,42015,6,0,B|168:152|288:72,1,180\n"
  , "312,168,42547,2,0,B|392:264|272:344,1,180\n"
  , "240,256,43080,5,0\n"
  , "296,184,43258,1,0\n"
  , "368,240,43435,1,0\n"
  , "304,304,43613,2,0,B|328:352|400:360,1,90,0|4\n"
  , "408,280,43968,5,0\n"
  , "456,208,44145,2,0,B|488:152|456:104,1,90\n"
  , "392,72,44500,2,0,B|352:112|400:176,2,90\n"
  , "465,121,45033,1,0\n"
  , "386,154,45210,5,0\n"
  , "304,192,45388,2,0,B|256:160|272:96,1,90\n"
  , "200,56,45743,2,0,B|112:120|160:248,1,180\n"
  , "149,216,46275,1,0\n"
  , "232,176,46453,2,0,B|288:272|248:352,1,180,4|0\n"
  , "168,304,46985,5,8\n"
  , "250,347,47163,1,0\n"
  , "336,320,47340,1,0\n"
  , "416,288,47518,2,0,B|456:248|448:192,1,90,0|8\n"
  , "352,224,47873,2,0,B|288:136|336:40,1,180\n"
  , "240,72,48405,2,0,B|136:88,1,90,8|0\n"
  , "88,152,48760,2,0,B|192:184,1,90\n"
  , "240,240,49115,2,0,B|144:272,1,90,8|0\n"
  , "72,304,49471,6,0,B|144:384|272:344,1,180,4|8\n"
  , "236,353,50003,1,0\n"
  , "320,312,50181,2,0,B|248:232|120:272,1,180,0|8\n"
  , "155,262,50713,5,0\n"
  , "112,104,50891,1,2\n"
  , "272,72,51068,1,0\n"
  , "320,224,51246,1,10\n"
  , "232,200,51423,5,0\n"
  , "288,128,51601,1,2\n"
  , "376,152,51778,1,8\n"
  , "320,224,51956,1,10\n"
  , "288,128,52133,1,0\n"
  , "320,224,52311,6,0,B|256:320|136:248,1,180,4|8\n"
  , "72,240,52843,2,0,B|136:144|256:216,1,180\n"
  , "320,224,53376,1,8\n"
  , "400,184,53553,2,0,B|400:-8,1,180\n"
  , "400,4,54086,5,8\n"
  , "464,152,54263,1,0\n"
  , "320,224,54441,1,0\n"
  , "248,80,54618,1,0\n"
  , "328,40,54796,5,8\n"
  , "320,128,54973,1,0\n"
  , "248,80,55151,2,0,B|48:80,2,180,4|8|0\n"
  , "320,128,56039,1,0\n"
  , "328,40,56216,1,8\n"
  , "400,96,56394,2,0,B|480:168|400:264,1,180\n"
  , "336,312,56926,1,8\n"
  , "248,336,57104,1,0\n"
  , "160,344,57281,2,0,B|112:360|48:296,2,90,0|0|8\n"
  , "208,264,57814,6,0,B|312:240|336:144,1,180,4|0\n"
  , "248,144,58346,2,0,B|200:112|208:56,1,90,8|0\n"
  , "136,128,58701,2,0,B|136:232,2,90,0|0|8\n"
  , "206,68,59234,5,0\n"
  , "232,152,59411,1,0\n"
  , "144,128,59589,1,0\n"
  , "206,68,59766,5,10\n"
  , "184,152,59944,1,0\n"
  , "168,240,60121,2,0,B|152:288|160:336,2,90,2|8|10\n"
  , "184,152,60654,1,0\n"
  , "256,200,60831,6,0,B|328:120|264:24,1,180,4|8\n"
  , "184,152,61364,2,0,B|112:232|184:328,1,180\n"
  , "175,316,61897,5,8\n"
  , "264,288,62074,2,0,B|336:232|440:248,1,180\n"
  , "488,176,62607,2,0,B|440:152|384:160,1,90,8|0\n"
  , "256,192,62962,5,0\n"
  , "256,192,63139,1,2\n"
  , "256,192,63317,1,2\n"
  , "144,128,63672,6,0,B|112:16,2,90,4|0|8\n"
  , "72,176,64204,2,0,B|160:256|240:144,1,180\n"
  , "312,192,64737,1,8\n"
  , "400,224,64914,2,0,B|312:160|232:264,1,180\n"
  , "184,320,65447,6,0,B|128:336|88:272,1,90,10|0\n"
  , "56,216,65802,2,0,B|24:168|72:104,2,90,2|0|10\n"
  , "103,293,66334,1,0\n"
  , "176,232,66512,2,0,B|256:152|360:232,1,180,0|8\n"
  , "416,264,67044,2,0,B|336:344|232:264,1,180\n"
  , "176,232,67577,1,8\n"
  , "128,152,67755,2,0,B|40:232|120:320,1,180\n"
  , "192,352,68287,2,0,B|240:384|288:360,1,90,10|0\n"
  , "352,320,68642,2,0,B|400:304|448:320,2,90,2|8|10\n"
  , "280,264,69175,6,0,B|184:184|288:88,1,180,4|0\n"
  , "328,40,69707,2,0,B|424:120|320:216,1,180,8|0\n"
  , "264,248,70240,2,0,B|224:280|152:248,1,90,0|8\n"
  , "216,176,70595,6,0,B|136:112|48:176,1,180\n"
  , "96,248,71127,2,0,B|176:312|264:248,1,180,8|0\n"
  , "344,288,71660,2,0,B|344:376,2,90,0|8|0\n"
  , "344,288,72192,5,2\n"
  , "240,192,72370,1,0\n"
  , "344,88,72547,1,10\n"
  , "448,192,72725,1,0\n"
  , "344,256,72902,5,2\n"
  , "280,192,73080,1,0\n"
  , "344,128,73258,1,10\n"
  , "408,192,73435,1,0\n"
  , "344,192,73613,5,2\n"
  , "344,192,73968,1,10\n"
  , "344,192,74323,1,2\n"
  , "208,280,74500,1,8\n"
  , "112,152,74678,1,10\n"
  , "248,56,74855,1,0\n"
  , "344,192,75033,5,4\n"
  , "256,208,75210,1,0\n"
  , "296,128,75388,1,8\n"
  , "344,192,75565,2,0,B|416:272|328:360,1,180\n"
  , "248,352,76098,1,8\n"
  , "160,352,76275,2,0,B|88:272|176:184,1,180\n"
  , "104,128,76808,6,0,B|96:64|16:40,1,90,10|0\n"
  , "16,136,77163,1,2\n"
  , "104,128,77340,2,0,B|152:160|216:104,1,90,0|10\n"
  , "144,48,77695,1,0\n"
  , "104,128,77873,2,0,B|16:216|104:320,1,180,0|8\n"
  , "128,368,78405,2,0,B|216:280|128:176,1,180\n"
  , "216,144,78938,1,8\n"
  , "296,96,79115,2,0,B|496:96,1,180\n"
  , "476,96,79648,5,10\n"
  , "328,24,79826,1,0\n"
  , "264,176,80003,1,2\n"
  , "408,248,80181,1,8\n"
  , "440,168,80358,5,10\n"
  , "408,248,80536,2,0,B|328:336|216:248,1,180,4|0\n"
  , "168,224,81068,2,0,B|248:136|360:224,1,180,8|0\n"
  , "392,136,81601,2,0,B|416:40,1,90,0|8\n"
  , "472,112,81956,6,0,B|496:320,1,180,4|0\n"
  , "416,336,82488,2,0,B|392:128,1,180,8|0\n"
  , "312,120,83021,2,0,B|312:224,2,90,0|8|0\n"
  , "312,120,83553,5,2\n"
  , "168,48,83731,1,0\n"
  , "88,184,83908,1,10\n"
  , "232,264,84086,1,0\n"
  , "240,176,84263,5,2\n"
  , "168,128,84441,1,0\n"
  , "160,216,84618,1,10\n"
  , "232,264,84796,2,0,B|312:352|400:256,1,180,4|0\n"
  , "387,268,85329,1,8\n"
  , "312,224,85506,6,0,B|232:136|144:232,1,180,0|8\n"
  , "88,280,86039,2,0,B|32:264|24:192,1,90,8|0\n"
  , "88,152,86394,6,0,B|168:64|272:104,1,180,4|8\n"
  , "328,136,86926,1,0\n"
  , "392,200,87104,1,2\n"
  , "256,192,87281,12,4,89234\n"
  , "160,88,100595,5,4\n"
  , "224,152,100772,2,0,B|264:120|256:56,2,90,0|8|0\n"
  , "160,88,101305,1,2\n"
  , "128,168,101482,1,0\n"
  , "184,240,101660,1,8\n"
  , "272,232,101837,1,0\n"
  , "224,152,102015,1,2\n"
  , "184,240,102192,2,0,B|216:344|336:304,1,180,0|0\n"
  , "384,240,102725,5,2\n"
  , "424,160,102902,1,0\n"
  , "400,72,103080,1,8\n"
  , "328,16,103258,1,0\n"
  , "240,0,103435,6,0,B|184:0|152:32,1,90,2|0\n"
  , "200,112,103790,1,8\n"
  , "272,160,103968,2,0,B|328:160|360:112,1,90,0|2\n"
  , "272,72,104323,1,0\n"
  , "200,112,104500,1,8\n"
  , "152,192,104678,2,0,B|112:232|136:296,2,90,0|2|0\n"
  , "224,248,105210,1,8\n"
  , "296,304,105388,2,0,B|376:384|488:288,1,180,2|0\n"
  , "459,310,105920,5,8\n"
  , "408,136,106098,1,0\n"
  , "336,88,106275,6,0,B|248:16|152:104,1,180,4|8\n"
  , "96,136,106808,2,0,B|184:208|280:120,1,180,2|0\n"
  , "336,88,107340,1,8\n"
  , "408,136,107518,1,0\n"
  , "360,216,107695,2,0,B|328:264|256:264,1,90,2|0\n"
  , "224,192,108050,2,0,B|144:272|240:360,2,180,8|2|8\n"
  , "280,64,109115,6,0,B|360:-8|448:72,1,180,2|8\n"
  , "360,112,109648,2,0,B|272:184|192:104,1,180,0|10\n"
  , "136,48,110181,1,8\n"
  , "72,112,110358,1,0\n"
  , "136,176,110536,1,2\n"
  , "200,112,110713,1,0\n"
  , "136,48,110891,5,10\n"
  , "136,176,111068,1,0\n"
  , "136,80,111246,1,2\n"
  , "136,208,111423,1,8\n"
  , "136,112,111601,1,10\n"
  , "136,240,111778,1,0\n"
  , "192,312,111956,6,0,B|224:352|288:352,2,90,4|0|8\n"
  , "272,272,112488,2,0,B|352:208|464:264,1,180,2|0\n"
  , "496,184,113021,1,8\n"
  , "416,136,113198,1,0\n"
  , "328,136,113376,1,2\n"
  , "248,176,113553,2,0,B|152:232|48:160,1,180\n"
  , "32,104,114086,5,2\n"
  , "76,177,114263,1,0\n"
  , "32,256,114441,1,8\n"
  , "76,177,114618,1,0\n"
  , "128,104,114796,6,0,B|168:0,2,90,2|0|8\n"
  , "176,184,115329,2,0,B|136:288,2,90,0|2|0\n"
  , "88,208,115861,1,8\n"
  , "120,296,116039,1,0\n"
  , "208,312,116216,6,0,B|248:336|304:320,2,90,2|0|8\n"
  , "264,240,116749,2,0,B|352:160|264:64,1,180,2|8\n"
  , "192,112,117281,1,8\n"
  , "278,80,117459,2,0,B|352:16|480:88,1,180,4|0\n"
  , "488,152,117991,5,8\n"
  , "400,144,118169,2,0,B|352:112|296:168,1,90,0|2\n"
  , "248,208,118524,1,0\n"
  , "168,248,118701,2,0,B|120:288|64:256,1,90,8|0\n"
  , "32,192,119056,1,2\n"
  , "88,120,119234,1,0\n"
  , "128,192,119411,6,0,B|184:208|224:128,2,90,8|0|2\n"
  , "160,64,119944,2,0,B|104:64|80:136,1,90,0|8\n"
  , "104,200,120299,1,0\n"
  , "184,248,120476,2,0,B|272:328|368:216,1,180,4|8\n"
  , "343,241,121009,1,0\n"
  , "264,192,121186,2,0,B|176:112|80:224,1,180,2|8\n"
  , "184,248,121719,1,0\n"
  , "264,192,121897,5,2\n"
  , "168,64,122074,1,0\n"
  , "40,160,122252,1,10\n"
  , "144,288,122429,1,0\n"
  , "184,208,122607,5,2\n"
  , "136,136,122784,1,8\n"
  , "96,216,122962,1,10\n"
  , "144,288,123139,1,0\n"
  , "144,288,123317,6,0,B|224:352|320:320,1,180,4|8\n"
  , "264,248,123849,2,0,B|352:192|472:240,1,180\n"
  , "464,312,124382,1,8\n"
  , "437,228,124559,2,0,B|504:144|432:32,1,180\n"
  , "384,120,125092,5,8\n"
  , "312,168,125269,2,0,B|264:128|296:56,1,90,0|0\n"
  , "384,120,125624,2,0,B|424:168|376:240,1,90,0|8\n"
  , "312,168,125979,1,0\n"
  , "224,168,126157,2,0,B|24:168,2,180,4|8|0\n"
  , "264,248,127044,1,0\n"
  , "312,168,127222,1,8\n"
  , "360,248,127400,2,0,B|448:176|344:72,1,180,0|0\n"
  , "312,168,127932,5,8\n"
  , "264,248,128110,1,0\n"
  , "216,328,128287,2,0,B|160:288|168:216,1,90,0|0\n"
  , "152,168,128642,1,8\n"
  , "80,216,128820,2,0,B|64:312|136:384,1,180,0|0\n"
  , "208,328,129352,5,8\n"
  , "296,304,129530,2,0,B|336:288|392:288,1,90\n"
  , "440,216,129885,2,0,B|392:192|320:200,1,90,0|8\n"
  , "264,184,130240,1,0\n"
  , "176,184,130417,2,0,B|128:208|80:176,1,90\n"
  , "88,184,130772,5,10\n"
  , "176,48,130950,1,0\n"
  , "312,136,131127,1,2\n"
  , "224,272,131305,1,8\n"
  , "184,192,131482,5,10\n"
  , "272,200,131660,1,0\n"
  , "224,272,131837,2,0,B|144:352|56:272,1,180,4|8\n"
  , "112,208,132370,2,0,B|192:128|280:208,1,180\n"
  , "336,264,132902,1,8\n"
  , "408,320,133080,2,0,B|496:264|456:136,1,180,0|8\n"
  , "440,72,133613,1,8\n"
  , "464,160,133790,1,0\n"
  , "376,128,133968,6,0,B|288:80|176:112,1,180,2|8\n"
  , "201,105,134678,1,2\n"
  , "256,192,134855,12,4,137518\n"
  , "128,192,146039,6,0,B|152:144|120:88,2,90,4|0|8\n"
  , "40,176,146571,2,0,B|-16:272|80:352,1,180\n"
  , "144,304,147104,1,8\n"
  , "224,256,147281,2,0,B|304:200|400:248,1,180\n"
  , "376,336,147814,5,10\n"
  , "464,304,147991,2,0,B|496:264|488:216,1,90,0|2\n"
  , "432,152,148346,1,0\n"
  , "344,128,148524,2,0,B|296:96|224:112,1,90,10|0\n"
  , "176,144,148879,6,0,B|128:176|80:152,2,90,4|0|8\n"
  , "144,64,149411,2,0,B|240:0|304:104,1,180\n"
  , "288,184,149944,1,8\n"
  , "248,264,150121,2,0,B|288:368|416:312,1,180\n"
  , "472,264,150654,5,10\n"
  , "312,264,150831,1,0\n"
  , "440,264,151009,1,2\n"
  , "280,264,151186,1,8\n"
  , "408,264,151364,1,10\n"
  , "248,264,151542,1,0\n"
  , "176,216,151719,6,0,B|152:112,2,90,4|0|8\n"
  , "96,264,152252,2,0,B|184:352|256:256,1,180\n"
  , "248,264,152784,1,8\n"
  , "328,216,152962,2,0,B|240:128|168:224,1,180\n"
  , "32,136,153494,6,0,B|8:32,1,90,10|0\n"
  , "120,216,153849,2,0,B|136:120,1,90,2|0\n"
  , "48,320,154204,2,0,B|0:232,1,90,10|0\n"
  , "168,208,154559,6,0,B|184:160|256:144,1,90,4|0\n"
  , "312,200,154914,1,8\n"
  , "248,264,155092,2,0,B|168:336|80:280,1,180,0|8\n"
  , "40,200,155624,2,0,B|104:184|152:232,1,90,8|0\n"
  , "256,192,155979,12,0,157400\n"
  , "56,256,160950,5,4\n"
  , "112,328,161127,1,0\n"
  , "168,256,161305,1,2\n"
  , "224,328,161482,2,0,B|272:296|256:232,1,90\n"
  , "208,176,162015,5,2\n"
  , "168,256,162192,1,0\n"
  , "256,248,162370,1,2\n"
  , "176,208,162547,1,0\n"
  , "224,280,162725,1,2\n"
  , "256,192,162902,1,0\n"
  , "208,120,163080,6,0,B|168:96|104:128,1,90,4|0\n"
  , "176,208,163613,2,0,B|224:240|272:208,1,90\n"
  , "328,160,164145,6,0,B|384:144,1,45,2|0\n"
  , "384,240,164500,2,0,B|448:232,1,45,2|0\n"
  , "400,320,164855,2,0,B|448:352,1,45,2|0\n"
  , "272,288,165210,6,0,B|224:320|168:304,1,90,4|0\n"
  , "128,240,165743,2,0,B|152:192|120:144,1,90,0|2\n"
  , "186,307,166275,1,2\n"
  , "125,153,166453,1,0\n"
  , "128,240,166630,6,0,B|80:256,2,45,0|0|0\n"
  , "208,200,167163,2,0,B|248:176|304:176,1,90,0|2\n"
  , "128,240,167695,1,2\n"
  , "293,176,167873,1,0\n"
  , "208,200,168050,6,0,B|160:216|112:176,2,90,4|0|2\n"
  , "240,120,168938,1,0\n"
  , "296,184,169115,1,0\n"
  , "352,120,169293,6,0,B|400:96|456:112,1,90\n"
  , "400,184,169826,1,2\n"
  , "312,192,170003,2,0,B|272:216|208:192,1,90\n"
  , "352,88,170536,6,0,B|400:64|456:80,1,90,2|0\n"
  , "312,240,171068,2,0,B|264:264|208:248,1,90\n"
  , "224,251,171601,1,4\n"
  , "88,128,172311,6,0,B|40:128,2,45,2|0|0\n"
  , "160,80,172843,2,0,B|192:128|160:184,1,90\n"
  , "104,216,173376,2,0,B|80:264,1,45,2|0\n"
  , "168,264,173731,2,0,B|168:320,1,45,2|0\n"
  , "248,272,174086,2,0,B|272:320,1,45,2|0\n"
  , "328,248,174441,6,0,B|336:152,1,90,4|0\n"
  , "408,200,174973,2,0,B|424:296,1,90\n"
  , "360,224,175506,1,2\n"
  , "392,144,175684,1,0\n"
  , "464,184,175861,1,2\n"
  , "376,184,176039,1,0\n"
  , "440,128,176216,1,2\n"
  , "448,216,176394,1,0\n"
  , "376,264,176571,6,0,B|336:296|280:272,1,90,4|0\n"
  , "216,240,177104,2,0,B|176:216|112:232,1,90\n"
  , "48,264,177636,1,2\n"
  , "40,96,177991,2,0,B|8:144|48:192,3,90,0|2|0|2\n"
  , "256,320,179411,5,0\n"
  , "384,192,179589,1,2\n"
  , "256,64,179766,1,2\n"
  , "128,192,179944,1,0\n"
  , "256,192,180121,12,0,182607\n"
  , "256,192,182962,12,0,184737\n"
  , "256,192,185092,5,0\n"
  , "256,192,185269,1,0\n"
  , "256,192,185447,1,0\n"
  , "88,120,185802,6,0,B|56:80|88:32,2,90,4|0|8\n"
  , "168,88,186334,2,0,B|248:16|344:80,1,180\n"
  , "333,73,186867,1,8\n"
  , "272,136,187044,2,0,B|192:208|96:144,1,180\n"
  , "32,200,187577,1,10\n"
  , "104,248,187755,2,0,B|144:272|200:264,1,90,0|2\n"
  , "280,240,188110,1,8\n"
  , "360,200,188287,2,0,B|400:160|464:168,1,90,10|0\n"
  , "392,88,188642,6,0,B|304:16|192:64,1,180,4|8\n"
  , "272,128,189175,2,0,B|184:184|88:120,1,180\n"
  , "32,72,189707,1,8\n"
  , "32,160,189885,2,0,B|32:352,1,180\n"
  , "112,296,190417,5,10\n"
  , "192,336,190595,1,0\n"
  , "256,272,190772,2,0,B|296:312|280:360,1,90,2|8\n"
  , "192,336,191127,1,10\n"
  , "176,248,191305,6,0,B|232:152|344:224,1,180,4|0\n"
  , "472,80,191837,2,0,B|416:176|304:104,1,180,8|0\n"
  , "317,112,192370,5,0\n"
  , "144,64,192547,1,8\n"
  , "56,96,192725,6,0,B|96:200|224:144,1,180\n"
  , "352,304,193258,2,0,B|312:200|184:256,1,180,8|0\n"
  , "204,247,193790,5,0\n"
  , "48,328,193968,2,0,B|16:288|32:232,1,90,8|0\n"
  , "29,243,194323,5,2\n"
  , "72,88,194500,1,0\n"
  , "232,120,194678,1,10\n"
  , "192,272,194855,1,0\n"
  , "112,232,195033,5,2\n"
  , "144,152,195210,1,0\n"
  , "224,192,195388,1,10\n"
  , "192,272,195565,2,0,B|256:352|376:312,1,180,0|8\n"
  , "400,240,196098,1,10\n"
  , "400,240,196187,1,0\n"
  , "400,240,196275,1,0\n"
  , "320,200,196453,6,0,B|296:160|312:104,2,90,2|8|10\n"
  , "400,240,196985,1,0\n"
  , "480,200,197163,2,0,B|506:238|482:286,2,90,4|0|8\n"
  , "400,240,197695,2,0,B|336:320|216:248,1,180\n"
  , "237,259,198228,1,8\n"
  , "160,224,198405,2,0,B|224:144|344:216,1,180\n"
  , "392,144,198938,6,0,B|416:48,1,90,10|0\n"
  , "472,128,199293,2,0,B|480:248,1,90,2|0\n"
  , "413,56,199648,1,10\n"
  , "477,217,199826,1,0\n"
  , "424,288,200003,6,0,B|352:352|248:312,1,180,4|8\n"
  , "296,240,200536,2,0,B|224:184|112:224,1,180\n"
  , "80,296,201068,1,8\n"
  , "80,296,201246,2,0,B|-8:232|48:104,1,180\n"
  , "96,64,201778,6,0,B|128:112|112:184,1,90,10|0\n"
  , "184,208,202133,2,0,B|200:120,1,90,2|8\n"
  , "272,64,202488,1,10\n"
  , "304,152,202666,6,0,B|320:336,1,180,4|0\n"
  , "400,288,203198,2,0,B|384:96,1,180,8|0\n"
  , "304,152,203731,1,0\n"
  , "272,64,203908,1,8\n"
  , "192,112,204086,2,0,B|-8:112,1,180,4|0\n"
  , "56,192,204618,2,0,B|248:192,1,180,8|0\n"
  , "192,112,205151,1,0\n"
  , "272,64,205329,1,8\n"
  , "304,152,205506,1,0\n"
  , "304,152,205684,5,2\n"
  , "184,256,205861,1,0\n"
  , "64,152,206039,1,10\n"
  , "184,40,206216,1,0\n"
  , "248,88,206394,5,2\n"
  , "248,216,206571,1,0\n"
  , "120,216,206749,1,10\n"
  , "120,88,206926,1,0\n"
  , "184,88,207104,5,2\n"
  , "248,152,207281,1,0\n"
  , "184,216,207459,1,10\n"
  , "120,152,207636,1,0\n"
  , "184,152,207814,5,2\n"
  , "184,152,207991,1,8\n"
  , "184,152,208169,1,10\n"
  , "416,320,208524,6,0,B|456:280|424:216,2,90,4|0|8\n"
  , "344,264,209056,2,0,B|272:192|160:240,1,180\n"
  , "104,288,209589,1,8\n"
  , "32,232,209766,2,0,B|0:192|32:128,2,90\n"
  , "104,288,210299,5,10\n"
  , "23,146,210476,1,0\n"
  , "112,176,210654,2,0,B|144:128|128:80,1,90,2|0\n"
  , "32,232,211009,1,10\n"
  , "130,90,211187,1,0\n"
  , "208,136,211364,6,0,B|224:200|288:232|376:184,1,180,4|8\n"
  , "456,352,211897,2,0,B|440:288|376:256|288:304,1,180\n"
  , "232,328,212429,1,8\n"
  , "152,296,212607,2,0,B|152:88,1,180\n"
  , "208,48,213139,6,0,B|256:80|232:152,1,90,10|0\n"
  , "312,184,213494,2,0,B|328:88,2,90,2|8|10\n"
  , "224,208,214027,6,0,B|272:312|400:248,1,180,4|0\n"
  , "464,240,214559,2,0,B|416:136|288:200,1,180,8|0\n"
  , "248,128,215092,2,0,B|216:32,1,90,0|8\n"
  , "160,112,215447,2,0,B|144:320,1,180\n"
  , "64,248,215979,2,0,B|80:56,1,180,8|0\n"
  , "160,112,216512,2,0,B|168:208,2,90,0|8|0\n"
  , "160,112,217044,5,2\n"
  , "320,72,217222,1,0\n"
  , "360,232,217400,1,10\n"
  , "208,280,217577,1,0\n"
  , "184,192,217755,5,2\n"
  , "272,168,217932,1,0\n"
  , "296,256,218110,1,10\n"
  , "208,280,218287,1,0\n"
  , "184,192,218465,2,2,B|0:192,2,180,2|10|2\n"
  , "184,192,219352,5,8\n"
  , "232,344,219530,1,10\n"
  , "384,288,219707,1,0\n"
  , "352,128,219885,1,4\n"
  , "288,184,220062,5,0\n"
  , "376,208,220240,1,8\n"
  , "352,128,220417,2,0,B|272:56|168:128,1,180\n"
  , "186,116,220950,1,8\n"
  , "152,200,221127,2,0,B|208:216|240:176,1,90\n"
  , "186,116,221482,1,0\n"
  , "152,200,221660,5,10\n"
  , "208,272,221837,1,0\n"
  , "296,304,222015,2,0,B|320:256|288:208,1,90,2|0\n"
  , "208,272,222370,1,10\n"
  , "296,304,222547,1,0\n"
  , "384,280,222725,6,0,B|464:208|400:104,1,180,4|8\n"
  , "360,40,223258,2,0,B|280:112|344:216,1,180\n"
  , "384,280,223790,1,8\n"
  , "296,304,223968,2,0,B|104:304,1,180\n"
  , "56,232,224500,1,10\n"
  , "112,160,224678,2,0,B|160:192|144:248,1,90,0|2\n"
  , "56,232,225033,1,8\n"
  , "24,144,225210,1,10\n"
  , "88,80,225388,6,0,B|176:40|280:72,1,180,4|0\n"
  , "208,136,225920,2,0,B|296:176|408:136,1,180,8|0\n"
  , "424,224,226453,2,0,B|448:320,1,90,0|8\n"
  , "360,288,226808,2,0,B|288:208|176:240,1,180\n"
  , "248,312,227340,2,0,B|160:376|56:304,1,180,8|0\n"
  , "40,232,227873,1,0\n"
  , "96,160,228050,2,0,B|144:208|128:272,1,90,8|0\n"
  , "200,184,228405,6,0,B|216:88,2,90,2|0|10\n"
  , "214,95,228938,1,0\n"
  , "288,144,229115,6,0,B|320:48,2,90,2|0|10\n"
  , "200,184,229648,2,0,B|264:272|360:200,1,180,4|0\n"
  , "432,152,230181,1,8\n"
  , "432,64,230358,2,0,B|432:264,1,180,0|8\n"
  , "432,336,230891,2,0,B|432:240,1,90,8|0\n"
  , "360,296,231246,6,0,B|272:344|184:288,1,180,4|8\n"
  , "128,232,231778,1,0\n"
  , "189,291,231956,1,2\n"
  , "256,192,232133,12,4,234086\n"
  ]