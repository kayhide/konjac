{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Text.Language.Type.Time where

import ClassyPrelude

import Data.Text.Language.Type.Base
import Data.Time (Day, LocalTime (..), TimeOfDay)


-- * Time related types

data Short
data Long

type LocalizeShortLong lang a =
  ( Localize lang a
  , Localize (lang, Short) a
  , Localize (lang, Long) a
  )

type LocalizeDay lang = LocalizeShortLong lang Day
type LocalizeTimeOfDay lang = LocalizeShortLong lang TimeOfDay

instance (Localize lang Day, Localize lang TimeOfDay) => Localize lang LocalTime where
  localize (LocalTime day tod) = localize @lang day <> " " <> localize @lang tod


-- * Integral wrappers

data AsYear i where
  AsYear :: Integral i => i -> AsYear i

data AsMonth i where
  AsMonth :: Integral i => i -> AsMonth i

data AsWeekDay i where
  AsWeekDay :: Integral i => i -> AsWeekDay i
