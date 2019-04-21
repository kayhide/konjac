module Data.Text.Language.Japanese
  ( Japanese(..)
  )
where

import ClassyPrelude

import Data.List ((!!))
import Data.Text.Language.Type
import Data.Time (Day (..), TimeOfDay (..))
import Data.Time.Format (TimeLocale (..), defaultTimeLocale, formatTime)

data Japanese = Japanese
  deriving (Eq, Show)

timeLocale :: TimeLocale
timeLocale = defaultTimeLocale
  { wDays =
      [ ("日曜日", "日")
      , ("月曜日", "月")
      , ("火曜日", "火")
      , ("水曜日", "水")
      , ("木曜日", "木")
      , ("金曜日", "金")
      , ("土曜日", "土")
      ]
  }

instance Localize Japanese Day where
  localize = pack . formatTime timeLocale "%Y/%m/%d"

instance Localize (Japanese, Short) Day where
  localize = pack . formatTime timeLocale "%m/%d"

instance Localize (Japanese, Long) Day where
  localize = pack . formatTime timeLocale "%Y年%m月%d日(%a)"


instance Localize Japanese TimeOfDay where
  localize = pack . formatTime timeLocale "%H:%M:%S"

instance Localize (Japanese, Short) TimeOfDay where
  localize = pack . formatTime timeLocale "%H:%M"

instance Localize (Japanese, Long) TimeOfDay where
  localize = pack . formatTime timeLocale "%H時%M分%S秒"


instance Localize Japanese (AsWeekDay i) where
  localize = localize @(Japanese, Short)

instance Localize (Japanese, Short) (AsWeekDay i) where
  localize (AsWeekDay i) =
    pack . snd . (wDays timeLocale !!) . fromIntegral $ i `mod` 7

instance Localize (Japanese, Long) (AsWeekDay i) where
  localize (AsWeekDay i) =
    pack . fst . (wDays timeLocale !!) . fromIntegral $ i `mod` 7
