{- |
The @Retry-After@ response header tells the client how long to wait before
making a follow-up request. It can accompany a @503 (Service Unavailable)@
or @429 (Too Many Requests)@ response, or give a minimum wait before
following a @3xx@ redirect. The value is either a number of seconds
(@delay-seconds@) or an HTTP-date.

@
Retry-After = HTTP-date / delay-seconds
@

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-10.2.3>

See also: "Network.HTTP.Headers.Refresh", "Network.HTTP.Headers.Sunset", "Network.HTTP.Headers.Date".
-}
module Network.HTTP.Headers.RetryAfter (
  RetryAfter (..),
  LargeDelay,
  largeDelayDigits,
  parseRetryAfterDelay,
  parseRetryAfterDelayWithin,
  retryAfterParser,
  renderRetryAfter,
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Time.Clock (UTCTime)
import Network.HTTP.Headers
import Network.HTTP.Headers.Date (dateParser, dateParserWith, renderDate)
import Network.HTTP.Headers.HeaderFieldName (hRetryAfter)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


data RetryAfter
  = DelaySeconds Word
  | SpecificTime UTCTime
  | LargeDelaySeconds !LargeDelay
  deriving stock (Eq, Show)


-- | A delay beyond the range of 'Word', stored as its decimal digits.
newtype LargeDelay = LargeDelay SBS.ShortByteString
  deriving stock (Eq, Show)


-- | Canonical ASCII digits with no leading zeros.
largeDelayDigits :: LargeDelay -> SBS.ShortByteString
largeDelayDigits (LargeDelay digits) = digits


-- | Parse a numeric delay in seconds, allowing HTTP field whitespace.
parseRetryAfterDelay :: BS.ByteString -> Either String RetryAfter
parseRetryAfterDelay digits = parseSingleFieldLine "Retry-After delay" delaySecondsParser (digits :| [])


-- | Parse a numeric delay with an inclusive limit in seconds.
-- A valid delay above the limit returns @Right Nothing@.
parseRetryAfterDelayWithin :: Word -> BS.ByteString -> Either String (Maybe Word)
parseRetryAfterDelayWithin limit wire =
  withShortDecimal wire (Right . withinLimit . fromIntegral) $
    parseSingleFieldLine "Retry-After delay" parser (wire :| [])
  where
    parser = (Just <$> boundedDecimalWord limit failed) <|> (Nothing <$ decimalDigitBytes)
    withinLimit seconds
      | seconds <= limit = Just seconds
      | otherwise = Nothing
{-# INLINE parseRetryAfterDelayWithin #-}


instance KnownHeader RetryAfter where
  type ParseFailure RetryAfter = String
  type Cardinality RetryAfter = 'ZeroOrOne
  type Direction RetryAfter = 'Response


  parseFromHeaders settings fields =
    case fields of
      wire :| [] -> withShortDecimal wire (Right . DelaySeconds . fromIntegral) fallback
      _ -> fallback
    where
      fallback = parseSingleFieldLine "Retry-After" (retryAfterParserWith (dateParserWith settings)) fields


  renderToHeaders _ = M.toStrictByteString . renderRetryAfter


  headerName _ = hRetryAfter


retryAfterParser :: ParserT st String RetryAfter
retryAfterParser = retryAfterParserWith dateParser


retryAfterParserWith :: ParserT st String UTCTime -> ParserT st String RetryAfter
retryAfterParserWith parseDate = delaySecondsParser <|> (SpecificTime <$> parseDate)


delaySecondsParser :: ParserT st String RetryAfter
delaySecondsParser =
  (DelaySeconds <$> boundedDecimalWord maxBound failed) <|> do
    -- The bounded parser consumes any finite prefix that fits. This
    -- alternative can succeed only when the value exceeded Word.
    digits <- byteStringOf decimalDigitBytes
    pure (LargeDelaySeconds (LargeDelay (SBS.toShort (BS.dropWhile (== 0x30) digits))))


renderRetryAfter :: RetryAfter -> M.Builder
renderRetryAfter = \case
  DelaySeconds seconds -> M.wordDec seconds
  SpecificTime time -> renderDate time
  LargeDelaySeconds delay -> M.shortByteString (largeDelayDigits delay)
