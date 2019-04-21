module Data.Text.Language.English
  ( English (..)
  )
where

import ClassyPrelude

import Data.List ((!!))
import Data.Text.Language.Type
import Data.Time.Format (TimeLocale (..), defaultTimeLocale, formatTime)
import Data.Time.LocalTime (TimeOfDay (..))

data English = English
  deriving (Eq, Show)

timeLocale :: TimeLocale
timeLocale = defaultTimeLocale


instance Localize English Day where
  localize = pack . formatTime timeLocale "%Y-%m-%d"

instance Localize (English, Long) Day where
  localize = pack . formatTime timeLocale "%B %d, %Y"

instance Localize (English, Short) Day where
  localize = pack . formatTime timeLocale "%b %d"


instance Localize English TimeOfDay where
  localize = pack . formatTime timeLocale "%H:%M:%S"

instance Localize (English, Long) TimeOfDay where
  localize = pack . formatTime timeLocale "%H:%M:%S"

instance Localize (English, Short) TimeOfDay where
  localize = pack . formatTime timeLocale "%H:%M"


instance Integral i => Localize (English, AsWeekDay) i where
  localize = localize @((English, Short), AsWeekDay)

instance Integral i => Localize ((English, Short), AsWeekDay) i where
  localize = pack . snd . (wDays timeLocale !!) . fromIntegral . (`mod` 7)

instance Integral i => Localize ((English, Long), AsWeekDay) i where
  localize = pack . fst . (wDays timeLocale !!) . fromIntegral . (`mod` 7)
