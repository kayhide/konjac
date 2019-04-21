{-# LANGUAGE AllowAmbiguousTypes #-}
module Data.Text.Language.Type where

import ClassyPrelude

import Data.Time.Calendar (Day)
import Data.Time (TimeOfDay)

data Abbr
data Short
data Long
data AsYear
data AsMonth
data AsWeekDay

class Localize lang a where
  localize :: a -> Text
  default localize :: Show a => a -> Text
  localize = tshow

instance Localize lang a => Localize (lang, ()) a where
  localize = localize @lang @a


type LocalizeShort lang a = Localize (lang, Short) a
type LocalizeLong lang a = Localize (lang, Long) a

type LocalizeShortLong lang a =
  ( Localize lang a
  , LocalizeShort lang a
  , LocalizeLong lang a
  )


type LocalizeDay lang = LocalizeShortLong lang Day
type LocalizeTimeOfDay lang = LocalizeShortLong lang TimeOfDay
