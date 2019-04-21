{-# LANGUAGE AllowAmbiguousTypes #-}
module DevMain where

import ClassyPrelude

import Data.Time.Calendar (fromGregorian)
import Data.Time.LocalTime (TimeOfDay(..))
import Data.Text.Language


(&) :: a -> (a -> b) -> b
(&) x f = f x

run :: IO ()
run = do
  say $ fromGregorian 2019 4 20 & localize @English
  say $ fromGregorian 2019 4 20 & localize @Japanese
  say $ fromGregorian 2019 4 20 & localize @(Japanese, Long)
  say $ TimeOfDay 15 25 0 & localize @Japanese
  say $ TimeOfDay 15 25 0 & localize @(Japanese, Short)
  say $ TimeOfDay 15 25 0 & localize @(Japanese, Long)
  say ""
  say $ (21 :: Int) & localize @((English, Long), AsWeekDay)
  say $ (21 :: Int) & localize @((English, Short), AsWeekDay)
  say $ (21 :: Int) & localize @((Japanese, Long), AsWeekDay)
  say $ (21 :: Int) & localize @((Japanese, Short), AsWeekDay)
  say ""

  let go (label, lang) = do
        say label
        inLang lang $ \ l -> do
          say $ TimeOfDay 15 25 0 & l
          say $ fromGregorian 2019 4 20 & l
        say ""
        say $ label <> " Short"
        inLang' @Short lang $ \ l -> do
          say $ TimeOfDay 15 25 0 & l
          say $ fromGregorian 2019 4 20 & l
        say ""
        say $ label <> " Long"
        inLang' @Long lang $ \ l -> do
          say $ TimeOfDay 15 25 0 & l
          say $ fromGregorian 2019 4 20 & l
        say ""

  traverse_ go ([("Japanese", "ja"), ("English", "en")] :: [(Text, Text)])




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
  _ -> f (localize @(English, len))

printDay
  :: forall lang. LocalizeDay lang
  => Day -> IO ()
printDay day = do
  say $ localize @lang day
  say $ localize @(lang, Short) day
  say $ localize @(lang, Long) day
