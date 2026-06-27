{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 7234 §5.5 @Warning@ — additional, often human-readable, information
about the status or transformation of a message that might not be
reflected in the status code.

== Grammar

@
Warning       = #warning-value
warning-value = warn-code SP warn-agent SP warn-text [ SP warn-date ]
warn-code     = 3DIGIT
warn-agent    = ( uri-host [ ":" port ] ) / pseudonym
warn-text     = quoted-string
warn-date     = DQUOTE HTTP-date DQUOTE
@

The @warn-code@ is kept as a 'Word' and always rendered as exactly three
digits. The @warn-agent@ is captured verbatim (it never contains
whitespace, DQUOTE, or a comma). The @warn-text@ is the decoded
quoted-string body, re-quoted on render. The optional @warn-date@ is an
HTTP-date.

The header was obsoleted by RFC 9111, which removed @Warning@; it is
parsed\/rendered faithfully here for compatibility with existing traffic.

See <https://www.rfc-editor.org/rfc/rfc7234.html#section-5.5>.

See also: "Network.HTTP.Headers.CacheControl", "Network.HTTP.Headers.Pragma", "Network.HTTP.Headers.Age", "Network.HTTP.Headers.Via".
-}
module Network.HTTP.Headers.Warning (
  Warning (..),
  WarningValue (..),
  warningParser,
  renderWarning,
) where

import Control.Monad.Combinators.NonEmpty (sepBy1)
import Data.ByteString (ByteString)
import Data.Char (digitToInt)
import Data.Foldable1 (fold1)
import Data.List.NonEmpty (NonEmpty)
import Data.Text.Short (ShortText)
import Data.Time.Clock (UTCTime)
import Network.HTTP.Headers
import Network.HTTP.Headers.Date (dateParser, renderDate)
import Network.HTTP.Headers.HeaderFieldName (hWarning)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- | A non-empty comma-separated list of warning-values.
newtype Warning = Warning {warningValues :: NonEmpty WarningValue}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup)


-- | A single @warning-value@.
data WarningValue = WarningValue
  { warnCode :: !Word
  , warnAgent :: !ShortText
  , warnText :: !RFC8941String
  , warnDate :: !(Maybe UTCTime)
  }
  deriving stock (Eq, Show)


instance KnownHeader Warning where
  type ParseFailure Warning = String
  type Cardinality Warning = 'ZeroOrMore
  type Direction Warning = 'RequestAndResponse


  parseFromHeaders _ headers = fold1 <$> traverse runWarningParser headers
  renderToHeaders _ = pure . M.toStrictByteString . renderWarning
  headerName _ = hWarning


runWarningParser :: ByteString -> Either String Warning
runWarningParser bs = case runParser warningParser bs of
  OK v "" -> Right v
  OK _ rest -> Left $ "Unconsumed input after parsing Warning header: " <> show rest
  Fail -> Left "Failed to parse Warning header"
  Err e -> Left e


warningParser :: ParserT st String Warning
warningParser =
  Warning <$> (ows *> (warningValueParser `sepBy1` (ows *> $(char ',') *> ows)) <* ows)


warningValueParser :: ParserT st String WarningValue
warningValueParser = do
  code <- warnCodeParser
  $(char ' ')
  agent <- warnAgentParser
  $(char ' ')
  text <- rfc8941String
  date <- optional ($(char ' ') *> warnDateParser)
  pure WarningValue {warnCode = code, warnAgent = agent, warnText = text, warnDate = date}


-- | Exactly three decimal digits, as required by @warn-code = 3DIGIT@.
warnCodeParser :: ParserT st e Word
warnCodeParser = do
  a <- digitVal
  b <- digitVal
  c <- digitVal
  pure (a * 100 + b * 10 + c)
  where
    digitVal = fromIntegral . digitToInt <$> satisfyAscii isDigit


-- | A @warn-agent@: a run of characters up to the next SP, DQUOTE, or comma.
warnAgentParser :: ParserT st e ShortText
warnAgentParser = shortASCIIFromParser_ (some (satisfyAscii isAgentChar))
  where
    isAgentChar ch = ch /= ' ' && ch /= '"' && ch /= ','


-- | @warn-date = DQUOTE HTTP-date DQUOTE@.
warnDateParser :: ParserT st String UTCTime
warnDateParser = $(char '"') *> dateParser <* $(char '"')


renderWarning :: Warning -> M.Builder
renderWarning = R.sepByCommas1 . fmap renderWarningValue . warningValues


renderWarningValue :: WarningValue -> M.Builder
renderWarningValue (WarningValue code agent text date) =
  renderWarnCode code
    <> M.char7 ' '
    <> R.shortText agent
    <> M.char7 ' '
    <> R.rfc8941String text
    <> maybe mempty (\d -> M.char7 ' ' <> renderWarnDate d) date


-- | Render a @warn-code@ zero-padded to three digits.
renderWarnCode :: Word -> M.Builder
renderWarnCode c
  | c < 10 = M.string8 "00" <> M.wordDec c
  | c < 100 = M.char7 '0' <> M.wordDec c
  | otherwise = M.wordDec c


renderWarnDate :: UTCTime -> M.Builder
renderWarnDate d = M.char7 '"' <> renderDate d <> M.char7 '"'
