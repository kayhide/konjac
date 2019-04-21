{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels #-}
module Data.Text.Language.Type where

import ClassyPrelude

import Data.Time (Day, LocalTime (..), TimeOfDay, ZonedTime)
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits (Symbol)


-- * Localize type class

class Localize lang a where
  localize :: a -> Text
  default localize :: Show a => a -> Text
  localize = tshow

instance Localize lang a => Localize (lang, ()) a where
  localize = localize @lang @a


-- * Time related types

data Short
data Long
data AsYear
data AsMonth
data AsWeekDay

type LocalizeShortLong lang a =
  ( Localize lang a
  , Localize (lang, Short) a
  , Localize (lang, Long) a
  )

type LocalizeDay lang = LocalizeShortLong lang Day
type LocalizeTimeOfDay lang = LocalizeShortLong lang TimeOfDay

instance (Localize lang Day, Localize lang TimeOfDay) => Localize lang LocalTime where
  localize (LocalTime day tod) = localize @lang day <> " " <> localize @lang tod


-- * Label

type family ToLanguage (l :: Symbol)

instance
  ( lang ~ ToLanguage l
  , Localize lang a
  ) => IsLabel l (a -> Text) where
  fromLabel = localize @lang
