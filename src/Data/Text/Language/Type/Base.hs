{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Text.Language.Type.Base
  ( Localize (..)
  , ToLanguage
  )
where

import ClassyPrelude

import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits (Symbol)


-- * Localize type class

class Localize lang a where
  localize :: a -> Text
  default localize :: Show a => a -> Text
  localize = tshow

instance Localize lang a => Localize (lang, ()) a where
  localize = localize @lang @a


-- * Label

type family ToLanguage (l :: Symbol)

instance
  ( lang ~ ToLanguage l
  , Localize lang a
  ) => IsLabel l (a -> Text) where
  fromLabel = localize @lang
