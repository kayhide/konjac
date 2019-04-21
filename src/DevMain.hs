{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLabels #-}
module DevMain where

import ClassyPrelude

import Data.Text.Language
import Data.Time (LocalTime (..), TimeOfDay (..), fromGregorian)


(&) :: a -> (a -> b) -> b
(&) x f = f x

run :: IO ()
run = do
  let date = fromGregorian 2019 4 20
  let time = TimeOfDay 15 25 0
  say $ date & localize @English
  say $ date & localize @Japanese
  say $ #ja date
  say $ #ja_long date
  say $ date & localize @(Japanese, Long)
  say $ time & localize @Japanese
  say $ time & localize @(Japanese, Short)
  say $ time & localize @(Japanese, Long)
  say $ LocalTime date time & localize @Japanese
  say $ LocalTime date time & localize @(Japanese, Short)
  say $ LocalTime date time & localize @(Japanese, Long)
  say ""
  say $ AsWeekDay (21 :: Int) & localize @(English, Long)
  say $ AsWeekDay (21 :: Int) & localize @(English, Short)
  say $ AsWeekDay (21 :: Int) & localize @(Japanese, Long)
  say $ AsWeekDay (21 :: Int) & localize @(Japanese, Short)
  say ""

  let go (label, lang) = do
        say label
        inLang lang $ \ l -> do
          say $ time & l
          say $ date & l
        say ""
        say $ label <> " Short"
        inLang' @Short lang $ \ l -> do
          say $ time & l
          say $ date & l
        say ""
        say $ label <> " Long"
        inLang' @Long lang $ \ l -> do
          say $ time & l
          say $ date & l
        say ""

  traverse_ go ([("Japanese", "ja"), ("English", "en")] :: [(Text, Text)])

type instance ToLanguage "en" = English
type instance ToLanguage "ja" = Japanese
type instance ToLanguage "ja_long" = (Japanese, Long)


inLang
  :: Text
  -> ((forall a.
       ( Localize English a
       , Localize Japanese a
       )
       => a
       -> Text
      ) -> r
     )
  -> r
inLang = inLang' @()

inLang'
  :: forall (len :: *) r.
     Text
  -> ((forall a.
       ( Localize (English, len) a
       , Localize (Japanese, len) a
       )
       => a
       -> Text
      ) -> r
     )
  -> r
inLang' lang f = case lang of
  "en" -> f (localize @(English, len))
  "ja" -> f (localize @(Japanese, len))
  _    -> f (localize @(English, len))
