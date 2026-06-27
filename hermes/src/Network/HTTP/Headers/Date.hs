{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- | The @Date@ header field carries the date and time at which the message was
originated, expressed as an HTTP-date (the preferred IMF-fixdate format). An
origin server with a clock is required to send it in responses; it may also
appear on requests that carry content. Caches and conditional requests compare
it against representation timestamps when reasoning about freshness and age.

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-6.6.1>

See also: "Network.HTTP.Headers.LastModified", "Network.HTTP.Headers.IfModifiedSince", "Network.HTTP.Headers.IfUnmodifiedSince", "Network.HTTP.Headers.ETag", "Network.HTTP.Headers.Expires".
-}
module Network.HTTP.Headers.Date (
  Date (..),
  dateParser,
  renderDate,
) where

import Control.Monad (when)
import qualified Data.ByteString.Char8 as C
import qualified Data.List.NonEmpty as NE
import Data.Time
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


newtype Date = Date
  { date :: UTCTime
  }
  deriving stock (Eq, Show)


instance KnownHeader Date where
  type ParseFailure Date = String
  type Cardinality Date = 'ZeroOrOne
  type Direction Date = 'Response


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser dateParser header of
      OK d "" -> Right $ Date d
      OK _ rest -> Left $ "Unconsumed input after parsing Date header: " <> show rest
      Fail -> Left "Failed to parse Date header"
      Err err -> Left err


  renderToHeaders _ = C.pack . formatTime defaultTimeLocale rfc822DateFormat . date


  headerName _ = hDate


shortDayOfWeek :: ParserT st err DayOfWeek
shortDayOfWeek =
  $( switch
      [|
        case _ of
          "Mon" -> pure Monday
          "Tue" -> pure Tuesday
          "Wed" -> pure Wednesday
          "Thu" -> pure Thursday
          "Fri" -> pure Friday
          "Sat" -> pure Saturday
          "Sun" -> pure Sunday
        |]
   )


longDayOfWeek :: ParserT st err DayOfWeek
longDayOfWeek =
  $( switch
      [|
        case _ of
          "Sunday" -> pure Sunday
          "Monday" -> pure Monday
          "Tuesday" -> pure Tuesday
          "Wednesday" -> pure Wednesday
          "Thursday" -> pure Thursday
          "Friday" -> pure Friday
          "Saturday" -> pure Saturday
        |]
   )


shortMonth :: ParserT st err MonthOfYear
shortMonth =
  $( switch
      [|
        case _ of
          "Jan" -> pure January
          "Feb" -> pure February
          "Mar" -> pure March
          "Apr" -> pure April
          "May" -> pure May
          "Jun" -> pure June
          "Jul" -> pure July
          "Aug" -> pure August
          "Sep" -> pure September
          "Oct" -> pure October
          "Nov" -> pure November
          "Dec" -> pure December
        |]
   )


dateParser :: ParserT st String UTCTime
dateParser = imfFixdate <|> obsDate
  where
    timeOfDay = do
      hour <- isolate 2 anyAsciiDecimalInt
      $(char ':')
      minute <- isolate 2 anyAsciiDecimalInt
      $(char ':')
      second <- isolate 2 anyAsciiDecimalInt
      pure $ TimeOfDay hour minute $ fromIntegral second
    imfFixdate = do
      _dayName <- shortDayOfWeek
      $(string ", ")
      day <- isolate 2 anyAsciiDecimalInt
      when (day < 1 || day > 31) failed
      $(char ' ')
      month <- shortMonth
      $(char ' ')
      year <- isolate 4 anyAsciiDecimalInteger
      $(char ' ')
      tod <- timeOfDay
      $(string " GMT")
      pure $ UTCTime (fromGregorian year month day) $ timeOfDayToTime tod

    obsDate = rfc850Date <|> asctimeDate
    rfc850Date = do
      _dayName <- longDayOfWeek
      $(string ", ")
      day <- isolate 2 anyAsciiDecimalInt
      when (day < 1 || day > 31) failed
      $(char '-')
      month <- shortMonth
      $(char '-')
      year <- isolate 2 anyAsciiDecimalInteger
      $(char ' ')
      tod <- timeOfDay
      $(string " GMT")
      -- RFC 6265 clarifies that the year should be adjusted as follows:
      let adjustedYear = if year < 70 then year + 2000 else year + 1900
      pure $ UTCTime (fromGregorian adjustedYear month day) $ timeOfDayToTime tod
    -- ANSI C's asctime() format
    asctimeDate = do
      _day <- shortDayOfWeek
      $(char ' ')
      month <- shortMonth
      $(char ' ')
      date <- isolate 2 (anyAsciiDecimalInt <|> ($(char ' ') *> anyAsciiDecimalInt))
      $(char ' ')
      tod <- timeOfDay
      $(char ' ')
      year <- isolate 4 anyAsciiDecimalInteger
      pure $ UTCTime (fromGregorian year month date) $ timeOfDayToTime tod


renderDate :: UTCTime -> M.Builder
renderDate (UTCTime day time) =
  let (year, month, date) = toGregorian day
      (TimeOfDay hour minute second) = timeToTimeOfDay time
  in dayOfWeekStr day
      <> ", "
      <> M.intDecPadded 2 date
      <> " "
      <> monthOfYearStr month
      <> " "
      <> M.intDecPadded 4 (fromIntegral year)
      <> " "
      <> M.intDecPadded 2 hour
      <> ":"
      <> M.intDecPadded 2 minute
      <> ":"
      <> M.intDecPadded 2 (round second)
      <> " GMT"
  where
    dayOfWeekStr day = case dayOfWeek day of
      Monday -> "Mon"
      Tuesday -> "Tue"
      Wednesday -> "Wed"
      Thursday -> "Thu"
      Friday -> "Fri"
      Saturday -> "Sat"
      Sunday -> "Sun"
    monthOfYearStr month = case month of
      January -> "Jan"
      February -> "Feb"
      March -> "Mar"
      April -> "Apr"
      May -> "May"
      June -> "Jun"
      July -> "Jul"
      August -> "Aug"
      September -> "Sep"
      October -> "Oct"
      November -> "Nov"
      December -> "Dec"
