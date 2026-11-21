{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 7234 §5.5 @Warning@ — additional, often human-readable, information
about the status or transformation of a message that might not be
reflected in the status code.

== Grammar

@
Warning       = 1#warning-value
warning-value = warn-code SP warn-agent SP warn-text [ SP warn-date ]
warn-code     = 3DIGIT
warn-agent    = ( uri-host [ ":" port ] ) / pseudonym
warn-text     = quoted-string
warn-date     = DQUOTE HTTP-date DQUOTE
@

The @warn-code@ is kept as a 'Word' and always rendered as exactly three
digits. The @warn-agent@ retains its validated spelling. The @warn-text@
is the decoded quoted-string body, with Latin-1 code points for the
original wire octets. The optional @warn-date@ is an HTTP-date.

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

import qualified Data.ByteString.Builder.Prim.Internal as Prim
import Data.Char (ord)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Text.Short (ShortText)
import Data.Time.Clock (UTCTime)
import Data.Word (Word8)
import Foreign.Storable (pokeByteOff)
import Network.HTTP.Grammar.Authority (isWarningAgent, warningAgent)
import Network.HTTP.Headers
import Network.HTTP.Headers.Date (dateParser, dateParserWith, renderDate, renderDateChecked)
import Network.HTTP.Headers.HeaderFieldName (hWarning)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R
import qualified Wireform.Builder as B


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


  parseFromHeaders settings = parseFieldLine "Warning" (warningParserWith (dateParserWith settings)) . combineFieldLines ", "


  renderToHeaders _ = maybe [] (pure . M.toStrictByteString) . warningBuilder


  renderToHeadersChecked _ (Warning (first NE.:| rest)) = do
    entry <- checkedWarningValue first
    trailing <- foldr appendWarning (Just mempty) rest
    pure [M.toStrictByteString (entry <> trailing)]
    where
      appendWarning item nextWarning = do
        entry <- checkedWarningValue item
        trailing <- nextWarning
        pure (", " <> entry <> trailing)


  headerName _ = hWarning


warningParser :: ParserT st String Warning
warningParser = warningParserWith dateParser


warningParserWith :: ParserT st String UTCTime -> ParserT st String Warning
warningParserWith parseDate = Warning <$> httpList1 "Warning" (warningValueParser parseDate)


warningValueParser :: ParserT st String UTCTime -> ParserT st String WarningValue
warningValueParser parseDate = do
  code <- warnCodeParser
  $(char ' ')
  agent <- warningAgent
  $(char ' ')
  text <- RFC8941String . shortTextFromLatin1 <$> httpQuotedBytes
  date <- optional ($(char ' ') *> $(char '"') *> parseDate <* $(char '"'))
  pure WarningValue {warnCode = code, warnAgent = agent, warnText = text, warnDate = date}


-- | Exactly three decimal digits, as required by @warn-code = 3DIGIT@.
warnCodeParser :: ParserT st e Word
warnCodeParser = do
  a <- digitVal
  b <- digitVal
  c <- digitVal
  pure (a * 100 + b * 10 + c)
  where
    digitVal = fromIntegral . (subtract 48) . ord <$> satisfyAscii isDigit


-- | Render a Warning field. The builder is empty if any code falls
-- outside the three-digit range.
renderWarning :: Warning -> M.Builder
renderWarning = fromMaybe mempty . warningBuilder


warningBuilder :: Warning -> Maybe M.Builder
warningBuilder (Warning entries)
  | any ((> 999) . warnCode) entries = Nothing
  | otherwise = Just (R.renderSeparated ", " renderWarningValue (NE.toList entries))


renderWarningValue :: WarningValue -> M.Builder
renderWarningValue (WarningValue code agent (RFC8941String text) date) =
  renderWarningPrefix code agent text <> maybe mempty (\d -> M.char7 ' ' <> renderWarnDate d) date


checkedWarningValue :: WarningValue -> Maybe M.Builder
checkedWarningValue (WarningValue code agent (RFC8941String text) date)
  | code > 999 || not (isWarningAgent agent && isHttpQuotedText text) = Nothing
  | otherwise = do
      encodedDate <- traverse renderDateChecked date
      pure (renderWarningPrefix code agent text <> foldMap (\d -> " \"" <> d <> "\"") encodedDate)


renderWarningPrefix :: Word -> ShortText -> ShortText -> M.Builder
renderWarningPrefix code agent text =
  renderWarnCodePrefix code
    <> R.shortText agent
    <> M.char7 ' '
    <> R.renderHttpQuotedShortText text


-- The checked builder supplies a code in the range 0 through 999.
renderWarnCodePrefix :: Word -> M.Builder
renderWarnCodePrefix = B.primFixed $ Prim.fixedPrim 4 $ \code ptr -> do
  let (prefix, ones) = code `quotRem` 10
      (hundreds, tens) = prefix `quotRem` 10
      digit n = 48 + fromIntegral n :: Word8
  pokeByteOff ptr 0 (digit hundreds)
  pokeByteOff ptr 1 (digit tens)
  pokeByteOff ptr 2 (digit ones)
  pokeByteOff ptr 3 (32 :: Word8)


renderWarnDate :: UTCTime -> M.Builder
renderWarnDate d = M.char7 '"' <> renderDate d <> M.char7 '"'
