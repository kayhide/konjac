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


instance Localize English (AsWeekDay i) where
  localize = localize @(English, Short)

instance Localize (English, Short) (AsWeekDay i) where
  localize (AsWeekDay i) =
    pack . snd . (wDays timeLocale !!) . fromIntegral $ i `mod` 7

instance Localize (English, Long) (AsWeekDay i) where
  localize (AsWeekDay i) =
    pack . fst . (wDays timeLocale !!) . fromIntegral $ i `mod` 7
