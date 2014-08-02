{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
module Game.Osu.OszLoader.OsuParser.Utils where

import Control.Applicative
import Data.Text
import Data.Attoparsec.Text

-- $setup
-- >>> import Control.Applicative
-- >>> import Data.Attoparsec.Text
-- >>> import Data.Text
-- >>> import Test.QuickCheck
-- >>> :set -XOverloadedStrings
-- >>> :set -XUnicodeSyntax

-- |
-- >>> parseOnly takeRestOfLine "hello"
-- Right "hello"
takeRestOfLine ∷ Parser Text
takeRestOfLine = takeTill isEndOfLine

{- |
>>> parseOnly boolIntParser "0"
Right False
>>> parseOnly boolIntParser "1"
Right True
>>> parseOnly boolIntParser "9"
Left "Failed reading: takeWith"
-}
boolIntParser ∷ Parser Bool
boolIntParser = "0" *> return False
                <|> "1" *> return True
