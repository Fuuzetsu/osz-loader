{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Osu.OszLoader.Types where

import Data.Map (Map)
import Data.Text (Text)

data General = General
  { _audioFilename ∷ FilePath
  , _audioLeadIn ∷ Int
  , _previewTime ∷ Int
  , _countdown ∷ Bool
  , _sampleSet ∷ Text
  , _stackLeniency ∷ Double
  , _mode ∷ Int
  , _letterboxInBreaks ∷ Bool
  , _widescreenStoryboard ∷ Maybe Bool
  } deriving (Show, Eq)

data Editor = Editor
  { _bookmarks ∷ [Int]
  , _distanceSpacing ∷ Double
  , _beatDivisor ∷ Int
  , _gridSize ∷ Int
  , _timelineZoom ∷ Maybe Int
  } deriving (Show, Eq)

data Metadata = Metadata
  { _title ∷ Text
  , _titleUnicode ∷ Text
  , _artist ∷ Text
  , _artistUnicode ∷ Text
  , _creator ∷ Text
  , _version ∷ Text
  , _source ∷ Text
  , _tags ∷ [Text]
  , _beatmapId ∷ Int
  , _beatmapSetId ∷ Int
  } deriving (Show, Eq)

data Difficulty = Difficulty
  { _hpDrainRate ∷ Int
  , _circleSize ∷ Int
  , _overallDifficulty ∷ Int
  , _approachRate ∷ Int
  , _sliderMultiplier ∷ Double
  , _sliderTickRate ∷ Double
  } deriving (Show, Eq)

type EventCommand = Text

data EventLayer = Background | Fail | Pass | Foreground
                deriving (Show, Eq)

data EventOrigin = TopLeft | TopCentre | TopRight
                 | CentreLeft | Centre | CentreRight
                 | BottomLeft | BottomCentre | BottomRight
                 deriving (Show, Eq)

data EventLoopType = LoopForever | LoopOnce
                   deriving (Show, Eq)

data EventObject = BasicImage { _layer ∷ EventLayer
                              , _origin ∷ EventOrigin
                              , _filePath ∷ FilePath
                              , _eventX ∷ Int
                              , _eventY ∷ Int
                              }
                 | MovingImage { _layer ∷ EventLayer
                               , _origin ∷ EventOrigin
                               , _filePath ∷ FilePath
                               , _eventX ∷ Int
                               , _eventY ∷ Int
                               , _frameCount ∷ Int
                               , _frameDelay ∷ Int
                               , _looptype ∷ EventLoopType
                               }
                 deriving (Show, Eq)

data Events = Events [(EventObject, [EventCommand])]
            deriving (Show, Eq)

data TimingPoint = TimingPoint
  { _offset ∷ Int
  , _millisecondsPerBeat ∷ Double
  , _meter ∷ Int
  , _sampleType ∷ Int
  , _timingSampleSet ∷ Int
  , _volume ∷ Int
  , _kiaiMode ∷ Bool
  , _inherited ∷ Bool
  } deriving (Show, Eq)

data Colours = Colours { _combo ∷ Map Int (Int, Int, Int) }
               deriving (Show, Eq)

-- | What the raw data means does not seem to be documented so I'm
-- just dumping raw data until I figure it out.
data HitObject = Circle { _hitObjectRawData ∷ Text }
               | Slider { _hitObjectRawData ∷ Text }
               | Spinner { _hitObjectRawData ∷ Text }
               deriving (Show, Eq)

data OsuMap = OsuMap
  { _formatVersion ∷ Int
  , _general ∷ General
  , _editor ∷ Editor
  , _metadata ∷ Metadata
  , _difficulty ∷ Difficulty
  , _timingPoints ∷ [TimingPoint]
  , _colours ∷ Colours
  , _hitObjects ∷ [HitObject]
  } deriving (Show, Eq)

data OsuFile = OsuFile { _originalFilename ∷ Text
                       , _osuMap ∷ OsuMap
                       } deriving (Show, Eq)
