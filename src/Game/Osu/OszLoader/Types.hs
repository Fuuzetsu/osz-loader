{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
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
  , _epilepsyWarning ∷ Maybe Bool
  , _letterboxInBreaks ∷ Bool
  , _widescreenStoryboard ∷ Maybe Bool
  } deriving (Show, Eq)

data Editor = Editor
  { _bookmarks ∷ [Int]
  , _distanceSpacing ∷ Double
  , _beatDivisor ∷ Int
  , _gridSize ∷ Int
  , _timelineZoom ∷ Maybe Double
  } deriving (Show, Eq)

data Metadata = Metadata
  { _title ∷ Text
  , _titleUnicode ∷ Maybe Text
  , _artist ∷ Text
  , _artistUnicode ∷ Maybe Text
  , _creator ∷ Text
  , _version ∷ Text
  , _source ∷ Text
  , _tags ∷ [Text]
  , _beatmapId ∷ Maybe Int
  , _beatmapSetId ∷ Maybe Int
  } deriving (Show, Eq)

data Difficulty = Difficulty
  { _hpDrainRate ∷ Int
  , _circleSize ∷ Double
  , _overallDifficulty ∷ Int
  , _approachRate ∷ Maybe Int
  , _sliderMultiplier ∷ Double
  , _sliderTickRate ∷ Double
  } deriving (Show, Eq)

type EventCommand = Text

data EventBackground = StaticBg Int Int FilePath (Maybe Int) (Maybe Int)
                     | VideoBg Int FilePath
                     deriving (Show, Eq)

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

data EventSample = EventSample { _evSample1 ∷ Int
                               , _evSample2 ∷ Int
                               , _sampleFilePath ∷ FilePath
                               , _sampleVolume ∷ Int
                               } deriving (Show, Eq)

type EventsBackgroundColour = (Int, Int, Int, Int, Int)
type EventBreakPeriod = (Int, Int, Int)

data Events = Events { _backgroundEvents ∷ [EventBackground]
                     , _breakPeriods ∷ [EventBreakPeriod]
                     , _storyboardEvents ∷ [(EventObject, [EventCommand])]
                     , _eventSamples ∷ [EventSample]
                     , _colourTransformations ∷ [EventsBackgroundColour]
                     } deriving (Show, Eq)

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

data Colours = Colours { _combo ∷ Map Int (Int, Int, Int)
                       , _sliderBorder ∷ Maybe (Int, Int, Int)
                       -- ^ spotted in v9
                       }
               deriving (Show, Eq)

-- | What the raw data means does not seem to be documented so I'm
-- just dumping it here until I figure it out
data HitObject = Circle (Int, Int, Int, Int, Int, (Int, Int, Int, Int))
               | Slider (Int, Int, Int, Int, Int, Text)
               | Spinner (Int, Int, Int, Int, Int, Maybe Int)
               deriving (Show, Eq)

data OsuMap = OsuMap
  { _formatVersion ∷ Int
  , _general ∷ General
  , _editor ∷ Editor
  , _metadata ∷ Metadata
  , _difficulty ∷ Difficulty
  , _events ∷ Events
  , _timingPoints ∷ [TimingPoint]
  , _colours ∷ Colours
  , _hitObjects ∷ [HitObject]
  } deriving (Show, Eq)

data OsuFile = OsuFile { _originalFilename ∷ Text
                       , _osuMap ∷ OsuMap
                       } deriving (Show, Eq)
