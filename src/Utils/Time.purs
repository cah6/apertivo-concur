module Utils.Time where

import Custom.Prelude
import Data.Enum (toEnum)
import Data.Int (round)
import Data.JSDate (JSDate, getHours, getMinutes, getTimezoneOffset)
import Data.JSDate as JSDate
import Data.Time (Time(..))
import Data.Time as Time
import Data.Time.Duration as Duration
import Effect.Now (nowDateTime)

localTimeToUtcJsDate :: Time -> Effect JSDate
localTimeToUtcJsDate localTime = do
  DateTime date time <- liftEffect $ nowDateTime
  nowVal <- liftEffect JSDate.now
  offsetVal <- liftEffect $ getTimezoneOffset nowVal
  let
    Tuple _ inputTimeUtc = Time.adjust (Duration.Minutes offsetVal) localTime
  let
    myDateTime = DateTime date inputTimeUtc
  pure (JSDate.fromDateTime myDateTime)

jsDateToTime :: JSDate -> Effect Time
jsDateToTime jsdate = do
  hr <- getHours jsdate
  min <- getMinutes jsdate
  -- I don't know how JSDate's hour and minute could be invalid. But just
  -- return zeroes if that is the case.
  case Tuple (toEnum (round hr)) (toEnum (round min)) of
    Tuple (Just hrTyped) (Just minTyped) -> pure $ Time hrTyped minTyped bottom bottom
    _ -> pure $ Time bottom bottom bottom bottom
 -- infixr 6 type Tuple as / -- type S = SProxy -- type Locale = SProxy "UTC" / S "Local" -- foreign import kind Locale -- foreign import data Local :: Locale -- foreign import data UTC :: Locale -- type Time' (locale :: Locale) = Time
