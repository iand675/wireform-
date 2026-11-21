{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
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
  dateParserAt,
  dateParserWith,
  cookieExpiryDateParser,
  renderDate,
  renderDateBytesChecked,
  cookieExpiryWriterChecked,
  renderDateChecked,
) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString.Builder.Prim as P
import qualified Data.ByteString.Builder.Prim.Internal as PI
import qualified Data.ByteString.Internal as BS
import Data.Char (ord)
import Data.Time
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (pokeByteOff)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Settings (headerDateCaseInsensitive, headerDateReference)
import qualified Wireform.Builder as B
import Wireform.Parser.Internal (Parser (..))


newtype Date = Date
  { date :: UTCTime
  }
  deriving stock (Eq, Show)


instance KnownHeader Date where
  type ParseFailure Date = String
  type Cardinality Date = 'ZeroOrOne
  type Direction Date = 'RequestAndResponse


  parseFromHeaders settings = parseSingleFieldLine "Date" (Date <$> dateParserWith settings)


  renderToHeaders _ = renderDateWith civilEpoch encodeDateParts B.toStrictByteString . date


  renderToHeadersChecked _ = renderDateBytesChecked . date


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
{-# NOINLINE longDayOfWeek #-}


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


{- | Accept all three HTTP-date formats. The clock-free entry point keeps
the conventional 1970–2069 window for RFC 850 dates.
-}
dateParser :: ParserT st String UTCTime
dateParser = dateParserUsing HttpDate Nothing


{- | Resolve RFC 850 two-digit years against an explicit reference clock,
as specified by RFC 9110 section 5.6.7.
-}
dateParserAt :: UTCTime -> ParserT st String UTCTime
dateParserAt = dateParserUsing HttpDate . Just


-- | Parse with the reference clock and case policy in the header settings.
dateParserWith :: HeaderSettings -> ParserT st String UTCTime
dateParserWith settings = dateParserUsing use (headerDateReference settings)
  where
    use
      | headerDateCaseInsensitive settings = CacheDate
      | otherwise = HttpDate


{- | Parse an RFC 6265 cookie-date up to the end of the Expires attribute.
The weekday and timezone text are informational. The common HTTP-style
spellings use the fixed-format parser before falling back to date tokens.
-}
cookieExpiryDateParser :: ParserT st String UTCTime
cookieExpiryDateParser =
  (dateParserUsing CookieExpiry Nothing <* lookahead (ows *> cookieDateEnd))
    <|> cookieDateTokens


cookieDateEnd :: ParserT st String ()
cookieDateEnd = eof <|> lookahead $(char ';')
{-# INLINE cookieDateEnd #-}


-- RFC 6265 §5.1.1 takes the first syntactic match for each component,
-- then validates the collected date. Commit a token before scanning the next.
cookieDateTokens :: ParserT st String UTCTime
cookieDateTokens = resume (-1) (-1) (-1) (-1)
  where
    resume day month year clock = Parser (collect day month year clock)
    {-# INLINE resume #-}

    collect !day !month !year !clock env end cursor state =
      runParser# step env end cursor state
      where
        step = do
          skipMany cookieDelimiter
          missing clock cookieTime (resume day month year) $
            missing day cookieDay (\value -> resume value month year clock) $
              missing month cookieMonth (\value -> resume day value year clock) $
                missing year cookieYear (\value -> resume day month value clock) $
                  branch (skipSome cookieTokenByte) (resume day month year clock) $
                    cookieDateEnd *> validate day month year clock

    missing current parser found rest
      | current < 0 = withOption parser found rest
      | otherwise = rest

    validate day month year clock
      | day < 1 || month < 1 || resolvedYear < 1601 || clock < 0 = failed
      | hour > 23 || minute > 59 || second > 59 = failed
      | otherwise =
          case fromGregorianValid (toInteger resolvedYear) month day of
            Just calendarDay -> pure (UTCTime calendarDay (secondsToDiffTime (fromIntegral (hour * 3600 + minute * 60 + second))))
            Nothing -> failed
      where
        (hour, minuteSecond) = clock `quotRem` 10_000
        (minute, second) = minuteSecond `quotRem` 100
        resolvedYear
          | year < 0 = year
          | year < 70 = 2000 + year
          | year < 100 = 1900 + year
          | otherwise = year


cookieDelimiter :: ParserT st String ()
cookieDelimiter = skipSatisfyAscii $ \c ->
  c == '\t' || (' ' <= c && c <= '~' && c /= ';' && not (isDigit c || isLatinLetter c || c == ':'))
{-# INLINE cookieDelimiter #-}


cookieTokenByte :: ParserT st String ()
cookieTokenByte = withAnyWord8 $ \byte ->
  if byte >= 0x80 || (48 <= byte && byte <= 58) || (97 <= (byte .|. 32) && (byte .|. 32) <= 122)
    then pure ()
    else failed
{-# INLINE cookieTokenByte #-}


cookieDigit :: ParserT st String Int
cookieDigit = withSatisfyAscii isDigit (pure . subtract (ord '0') . ord)
{-# INLINE cookieDigit #-}


cookieDigitIfPresent :: Int -> ParserT st String Int
cookieDigitIfPresent prefix = withOption cookieDigit (\digit -> pure (prefix * 10 + digit)) (pure prefix)
{-# INLINE cookieDigitIfPresent #-}


cookieOneOrTwoDigits :: ParserT st String Int
cookieOneOrTwoDigits = cookieDigit >>= cookieDigitIfPresent
{-# INLINE cookieOneOrTwoDigits #-}


finishNumericToken :: ParserT st String ()
finishNumericToken = notFollowedBy (skipSatisfyAscii isDigit) *> skipMany cookieTokenByte
{-# INLINE finishNumericToken #-}


cookieDay :: ParserT st String Int
cookieDay = cookieOneOrTwoDigits <* finishNumericToken


cookieYear :: ParserT st String Int
cookieYear = do
  first <- cookieDigit
  second <- cookieDigit
  year <- cookieDigitIfPresent (10 * first + second) >>= cookieDigitIfPresent
  year <$ finishNumericToken


cookieTime :: ParserT st String Int
cookieTime = do
  hour <- cookieOneOrTwoDigits <* $(char ':')
  minute <- cookieOneOrTwoDigits <* $(char ':')
  second <- cookieOneOrTwoDigits <* finishNumericToken
  pure (hour * 10_000 + minute * 100 + second)


cookieMonth :: ParserT st String Int
cookieMonth = foldedShortMonth <* skipMany cookieTokenByte


-- Pack three ASCII letters into one word after setting their case bits.
foldedThreeLetters :: ParserT st err Word32
foldedThreeLetters = do
  firstTwo <- anyWord16be
  third <- anyWord8
  pure (fromIntegral firstTwo `shiftL` 8 .|. fromIntegral third .|. 0x20_20_20)
{-# INLINE foldedThreeLetters #-}


foldedShortMonth :: ParserT st err MonthOfYear
foldedShortMonth =
  foldedThreeLetters >>= \case
    0x6a_61_6e -> pure January
    0x66_65_62 -> pure February
    0x6d_61_72 -> pure March
    0x61_70_72 -> pure April
    0x6d_61_79 -> pure May
    0x6a_75_6e -> pure June
    0x6a_75_6c -> pure July
    0x61_75_67 -> pure August
    0x73_65_70 -> pure September
    0x6f_63_74 -> pure October
    0x6e_6f_76 -> pure November
    0x64_65_63 -> pure December
    _ -> failed


foldedShortDayOfWeek :: ParserT st err DayOfWeek
foldedShortDayOfWeek =
  foldedThreeLetters >>= \case
    0x6d_6f_6e -> pure Monday
    0x74_75_65 -> pure Tuesday
    0x77_65_64 -> pure Wednesday
    0x74_68_75 -> pure Thursday
    0x66_72_69 -> pure Friday
    0x73_61_74 -> pure Saturday
    0x73_75_6e -> pure Sunday
    _ -> failed


foldedLongDayOfWeek :: ParserT st err DayOfWeek
foldedLongDayOfWeek = do
  weekday <- foldedShortDayOfWeek
  case weekday of
    Tuesday -> four 's' 'd' 'a' 'y'
    Wednesday -> three 'n' 'e' 's' *> three 'd' 'a' 'y'
    Thursday -> four 'r' 's' 'd' 'a' *> lastY
    Saturday -> four 'u' 'r' 'd' 'a' *> lastY
    _ -> three 'd' 'a' 'y'
  pure weekday
  where
    three a b c = match foldedThreeLetters (asciiWord a b c '\0' `shiftR` 8)
    four a b c d = match ((.|. 0x20_20_20_20) <$> anyWord32be) (asciiWord a b c d)
    lastY = match ((.|. 0x20) <$> anyWord8) 121
    match parser expected = do
      value <- parser
      if value == expected then pure () else failed
{-# NOINLINE foldedLongDayOfWeek #-}


foldedGmt :: ParserT st err ()
foldedGmt = $(switchAsciiCI [|case _ of " GMT" -> pure ()|])
{-# NOINLINE foldedGmt #-}


data DateUse = HttpDate | CacheDate | CookieExpiry


dateLiteral :: DateUse -> a -> a -> a
dateLiteral CacheDate _ folded = folded
dateLiteral _ strict _ = strict
{-# INLINE dateLiteral #-}


dateParserUsing :: DateUse -> Maybe UTCTime -> ParserT st String UTCTime
dateParserUsing use reference = imfFixdate <|> rfc850Date <|> asctimeDate
  where
    digit = withSatisfyAscii isDigit (pure . subtract (fromEnum '0') . fromEnum)
    twoDigits = do
      bytes <- anyWord16be
      let first = (bytes `shiftR` 8) - 48
          second = (bytes .&. 0xff) - 48
      if first <= 9 && second <= 9
        then pure (fromIntegral (first * 10 + second))
        else failed
    fourDigits = do
      first <- twoDigits
      second <- twoDigits
      pure (toInteger (first * 100 + second))
    timeOfDay = do
      hour <- twoDigits
      $(char ':')
      minute <- twoDigits
      $(char ':')
      second <- twoDigits
      if hour < 24 && minute < 60 && (second < 60 || leapSecond hour minute second)
        then pure (secondsToDiffTime (fromIntegral (hour * 3600 + minute * 60 + second)))
        else failed
    leapSecond hour minute second = case use of
      CookieExpiry -> False
      _ -> second == 60 && hour == 23 && minute == 59
    weekdayShort = dateLiteral use shortDayOfWeek foldedShortDayOfWeek
    monthShort = dateLiteral use shortMonth foldedShortMonth
    gmt = dateLiteral use $(string " GMT") foldedGmt
    imfFixdate = do
      weekday <- weekdayShort
      $(string ", ")
      day <- twoDigits
      $(char ' ')
      month <- monthShort
      $(char ' ')
      year <- fourDigits
      $(char ' ')
      tod <- timeOfDay
      gmt
      validatedDate weekday year month day tod

    rfc850Date = do
      weekday <- dateLiteral use longDayOfWeek foldedLongDayOfWeek
      $(string ", ")
      day <- twoDigits
      $(char '-')
      month <- monthShort
      $(char '-')
      year <- toInteger <$> twoDigits
      $(char ' ')
      tod <- timeOfDay
      gmt
      validatedDate weekday (resolveYear year month day tod) month day tod
    -- ANSI C's asctime() format
    asctimeDate = do
      weekday <- weekdayShort
      $(char ' ')
      month <- monthShort
      $(char ' ')
      date <- twoDigits <|> ($(char ' ') *> digit)
      $(char ' ')
      tod <- timeOfDay
      $(char ' ')
      year <- fourDigits
      validatedDate weekday year month date tod

    validatedDate weekday year month day tod = case fromGregorianValid year month day of
      Just calendarDay | validYearAndWeekday calendarDay -> pure (UTCTime calendarDay tod)
      _ -> failed
      where
        validYearAndWeekday calendarDay = case use of
          CookieExpiry -> year >= 1601
          _ -> year >= 1900 && dayOfWeek calendarDay == weekday

    resolveYear year month day tod = case reference of
      Nothing -> if year < 70 then year + 2000 else year + 1900
      Just (UTCTime today now) ->
        let lastPossibleDay = addGregorianYearsClip 50 today
            (limitYear, limitMonth, limitDay) = toGregorian lastPossibleDay
            candidate = limitYear - limitYear `mod` 100 + year
        in if (candidate, month, day, tod) > (limitYear, limitMonth, limitDay, now)
             then candidate - 100
             else candidate


renderDate :: UTCTime -> M.Builder
renderDate = renderDateWith civilEpoch (B.primFixed fixedDate) id


-- | Render a date within the bounds of the HTTP-date parser.
renderDateChecked :: UTCTime -> Maybe M.Builder
renderDateChecked = renderDateWith firstHttpDateDay (Just . B.primFixed fixedDate) (const Nothing)


-- | Encode an HTTP-date as exactly 29 bytes when the timestamp is representable.
renderDateBytesChecked :: UTCTime -> Maybe BS.ByteString
renderDateBytesChecked = renderDateWith firstHttpDateDay (Just . encodeDateParts) (const Nothing)


-- | Write a cookie expiration as 29 bytes of IMF-fixdate. Cookie dates
-- start at 1601 and use seconds 00 through 59.
cookieExpiryWriterChecked :: UTCTime -> Maybe (Ptr Word8 -> IO ())
cookieExpiryWriterChecked time
  | utctDayTime time < 86_400 =
      renderDateWith firstCookieExpiryDay (Just . PI.runF fixedDate) (const Nothing) time
  | otherwise = Nothing


encodeDateParts :: DateParts -> BS.ByteString
encodeDateParts = BS.unsafeCreate 29 . PI.runF fixedDate
{-# INLINE encodeDateParts #-}


renderDateWith :: Integer -> (DateParts -> a) -> (M.Builder -> a) -> UTCTime -> a
renderDateWith earliest fixed variable (UTCTime day time) =
  let mjd = toModifiedJulianDay day
      picos = diffTimeToPicoseconds time
  in if mjd >= earliest && mjd <= lastFourDigitDay && picos >= 0 && picos < 86_401_000_000_000_000
       then
         let seconds = (fromInteger picos :: Word64) `quot` 1_000_000_000_000
         in fixed (dateParts (fromInteger mjd) (fromIntegral seconds))
       else variable (renderVariableDate day time)
{-# INLINE renderDateWith #-}


renderVariableDate :: Day -> DiffTime -> M.Builder
renderVariableDate day time =
  let (year, month, date) = toGregorian day
      (TimeOfDay hour minute second) = timeToTimeOfDay time
  in B.word32BE (weekdayPrefix (dayOfWeek day))
       <> " "
       <> M.intDecPadded 2 date
       <> " "
       <> B.word32BE (monthPrefix month)
       <> M.intDecPadded 4 (fromIntegral year)
       <> " "
       <> M.intDecPadded 2 hour
       <> ":"
       <> M.intDecPadded 2 minute
       <> ":"
       <> M.intDecPadded 2 (truncate second)
       <> " GMT"


data DateParts = DateParts
  { partsWeekday :: !DayOfWeek
  , partsYear :: {-# UNPACK #-} !Int
  , partsMonth :: {-# UNPACK #-} !Int
  , partsDay :: {-# UNPACK #-} !Int
  , partsSeconds :: {-# UNPACK #-} !Int
  }


-- Gregorian cycles start in March so the leap day ends the year.
civilEpoch, firstCookieExpiryDay, firstHttpDateDay, lastFourDigitDay :: Integer
civilEpoch = toModifiedJulianDay (fromGregorian 1600 3 1)
firstCookieExpiryDay = toModifiedJulianDay (fromGregorian 1601 1 1)
firstHttpDateDay = toModifiedJulianDay (fromGregorian 1900 1 1)
lastFourDigitDay = toModifiedJulianDay (fromGregorian 9999 12 31)


-- Howard Hinnant's civil_from_days on a bounded, nonnegative cycle count.
-- https://howardhinnant.github.io/date_algorithms.html#civil_from_days
dateParts :: Int -> Int -> DateParts
dateParts mjd seconds =
  let offset = mjd - fromInteger civilEpoch
      (cycleIndex, cycleDay) = offset `quotRem` 146_097
      cycleYear = (cycleDay - cycleDay `quot` 1460 + cycleDay `quot` 36_524 - cycleDay `quot` 146_096) `quot` 365
      yearDay = cycleDay - (365 * cycleYear + cycleYear `quot` 4 - cycleYear `quot` 100)
      marchMonth = (5 * yearDay + 2) `quot` 153
      monthDay = yearDay - (153 * marchMonth + 2) `quot` 5 + 1
      marchYear = 1600 + 400 * cycleIndex + cycleYear
      (year, month) =
        if marchMonth < 10 then (marchYear, marchMonth + 3) else (marchYear + 1, marchMonth - 9)
      weekday = toEnum ((mjd + 2) `mod` 7 + 1)
  in DateParts weekday year month monthDay seconds


-- One bounded write for the common 29-byte IMF-fixdate representation.
fixedDate :: P.FixedPrim DateParts
fixedDate = PI.fixedPrim 29 $ \DateParts {partsWeekday = weekday, partsYear = year, partsMonth = month, partsDay = day, partsSeconds = seconds} ptr -> do
  let putByte offset byte = pokeByteOff ptr offset (byte :: Word8)
      putPair offset value = PI.runF P.word16BE (decimalPair value) (ptr `plusPtr` offset)
      (yearHigh, yearLow) = year `quotRem` 100
      (hours, remaining) = min seconds 86_399 `quotRem` 3600
      (minutes, secondsInMinute) = remaining `quotRem` 60
      second = if seconds == 86_400 then 60 else secondsInMinute
  PI.runF P.word32BE (weekdayPrefix weekday) ptr
  putByte 4 32
  putPair 5 day
  putByte 7 32
  PI.runF P.word32BE (monthPrefix month) (ptr `plusPtr` 8)
  putPair 12 yearHigh
  putPair 14 yearLow
  putByte 16 32
  putPair 17 hours
  putByte 19 58
  putPair 20 minutes
  putByte 22 58
  putPair 23 second
  PI.runF P.word32BE (asciiWord ' ' 'G' 'M' 'T') (ptr `plusPtr` 25)


decimalPair :: Int -> Word16
decimalPair n =
  let (tens, ones) = n `quotRem` 10
  in fromIntegral ((tens + 48) `shiftL` 8 .|. (ones + 48))
{-# INLINE decimalPair #-}


weekdayPrefix :: DayOfWeek -> Word32
weekdayPrefix = \case
  Monday -> asciiWord 'M' 'o' 'n' ','
  Tuesday -> asciiWord 'T' 'u' 'e' ','
  Wednesday -> asciiWord 'W' 'e' 'd' ','
  Thursday -> asciiWord 'T' 'h' 'u' ','
  Friday -> asciiWord 'F' 'r' 'i' ','
  Saturday -> asciiWord 'S' 'a' 't' ','
  Sunday -> asciiWord 'S' 'u' 'n' ','


monthPrefix :: MonthOfYear -> Word32
monthPrefix month = case month of
  January -> asciiWord 'J' 'a' 'n' ' '
  February -> asciiWord 'F' 'e' 'b' ' '
  March -> asciiWord 'M' 'a' 'r' ' '
  April -> asciiWord 'A' 'p' 'r' ' '
  May -> asciiWord 'M' 'a' 'y' ' '
  June -> asciiWord 'J' 'u' 'n' ' '
  July -> asciiWord 'J' 'u' 'l' ' '
  August -> asciiWord 'A' 'u' 'g' ' '
  September -> asciiWord 'S' 'e' 'p' ' '
  October -> asciiWord 'O' 'c' 't' ' '
  November -> asciiWord 'N' 'o' 'v' ' '
  December -> asciiWord 'D' 'e' 'c' ' '


asciiWord :: Char -> Char -> Char -> Char -> Word32
asciiWord a b c d =
  fromIntegral (ord a) `shiftL` 24
    .|. fromIntegral (ord b) `shiftL` 16
    .|. fromIntegral (ord c) `shiftL` 8
    .|. fromIntegral (ord d)
