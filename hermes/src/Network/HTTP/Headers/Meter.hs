{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 2227 @Meter@ — a list of hit-metering and usage-limiting directives.
Full and abbreviated spellings may be mixed. Known directives use the
request, report, and response grammars from the RFC. Unknown tokens
remain available as opaque extensions for callers that forward them.

A present empty list remains a distinct value at the header API.
The RFC assigns it different defaults in requests and responses.

Spec: <https://www.rfc-editor.org/rfc/rfc2227.html#section-5>

See also: "Network.HTTP.Headers.CacheControl", "Network.HTTP.Headers.Age".
-}
module Network.HTTP.Headers.Meter (
  Meter (..),
  MeterDirective (..),
  MeterDirectiveKind (..),
  MeterUsageCounts (..),
  meterDirectiveKind,
  meterDirectiveFromKind,
  meterParser,
  renderMeter,
) where

import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Short.Internal as SBSI
import qualified Data.ByteString.Unsafe as BSU
import Data.Char (toLower)
import Data.Maybe (isNothing)
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Data.Word (Word8)
import Network.HTTP.Grammar.Parser
import qualified Network.HTTP.Grammar.Rendering as R
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hMeter)
import qualified Network.HTTP.Headers.Mason as M
import Numeric.Natural (Natural)
import Wireform.Encode.Direct (dWord8)


data MeterDirective = MeterDirective
  { meterName :: !ST.ShortText
  , meterValue :: !(Maybe ST.ShortText)
  }
  deriving stock (Eq, Show)


newtype Meter = Meter {meterDirectives :: [MeterDirective]}
  deriving stock (Eq, Show)


data MeterUsageCounts = MeterUsageCounts
  { meterUses :: !Natural
  , meterReuses :: !Natural
  }
  deriving stock (Eq, Show)


data MeterDirectiveKind
  = MeterWillReportAndLimit
  | MeterWontReport
  | MeterWontLimit
  | MeterCount !MeterUsageCounts
  | MeterMaxUses !Natural
  | MeterMaxReuses !Natural
  | MeterDoReport
  | MeterDontReport
  | MeterTimeoutMinutes !Natural
  | MeterWontAsk
  | MeterExtension !ST.ShortText !(Maybe ST.ShortText)
  deriving stock (Eq, Show)


data DirectiveGrammar
  = Flag !HeaderMessage !MeterDirectiveKind
  | Count
  | DecimalArgument !(Natural -> MeterDirectiveKind)
  | Extension


instance KnownHeader Meter where
  type ParseFailure Meter = String
  type Cardinality Meter = 'ZeroOrOne
  type Direction Meter = 'RequestAndResponse


  parseFromHeaders settings fields = Meter <$> parseListFieldLines "Meter" (meterList (headerMessage settings)) fields


  renderToHeaders _ = M.toStrictByteString . renderMeter


  renderToHeadersChecked settings (Meter directives) =
    R.encodeCommaSeparatedChecked bound write directives
    where
      direction = headerMessage settings
      allowed expected = maybe True (== expected) direction
      width = SBS.length . ST.toShortByteString
      add = BSI.checkedAdd "Meter"
      bound (MeterDirective name value)
        | isHttpTokenText name =
            let argumentSize = case textGrammar name of
                  Flag expected _ | allowed expected && isNothing value -> 0
                  Count | allowed HeaderRequest, Just v <- value, isDecimalPairText 47 v -> add 1 (width v)
                  DecimalArgument _ | allowed HeaderResponse, Just v <- value, isDecimalText v -> add 1 (width v)
                  Extension -> case value of
                    Nothing -> 0
                    Just v | isHttpQuotedText v -> add 3 (add (width v) (width v))
                    _ -> -1
                  _ -> -1
            in if argumentSize < 0 then -1 else add (width name) argumentSize
        | otherwise = -1
      write ptr start (MeterDirective name value) = do
        end <- R.copyText ptr start name
        case value of
          Nothing -> pure end
          Just text -> do
            equals <- dWord8 ptr end 61
            case textGrammar name of
              Extension -> R.writeParameterValue ptr equals text
              _ -> R.copyText ptr equals text


  headerName _ = hMeter


meterParser :: ParserT st String Meter
meterParser = Meter <$> meterList Nothing


meterList :: Maybe HeaderMessage -> ParserT st String [MeterDirective]
meterList direction = httpList $ do
  bytes <- rfc9110TokenBS
  ows
  let name = STU.fromByteStringUnsafe bytes
  value <- case directiveGrammar bytes of
    Flag expected _ -> Nothing <$ checkDirection expected
    Count -> checkDirection HeaderRequest *> (Just <$> (equals *> shortASCIIFromParser_ (digits *> $(char '/') *> digits)))
    DecimalArgument _ -> checkDirection HeaderResponse *> (Just <$> (equals *> shortASCIIFromParser_ digits))
    Extension -> withOption equals (const (Just <$> (rfc9110Token <|> (ST.fromText <$> httpQuotedString)))) (pure Nothing)
  pure (MeterDirective name value)
  where
    equals = $(char '=') *> ows
    digits = skipSome (skipSatisfyAscii isDigit)
    checkDirection expected
      | maybe True (== expected) direction = pure ()
      | otherwise = err "Meter directive is invalid in this message direction"


directiveGrammar :: ByteString -> DirectiveGrammar
directiveGrammar bytes
  | BS.length bytes == 1 = abbreviatedGrammar (BSU.unsafeHead bytes)
directiveGrammar bytes = case lower of
  "will-report-and-limit" -> request MeterWillReportAndLimit
  "wont-report" -> request MeterWontReport
  "wont-limit" -> request MeterWontLimit
  "count" -> Count
  "max-uses" -> DecimalArgument MeterMaxUses
  "max-reuses" -> DecimalArgument MeterMaxReuses
  "do-report" -> response MeterDoReport
  "dont-report" -> response MeterDontReport
  "timeout" -> DecimalArgument MeterTimeoutMinutes
  "wont-ask" -> response MeterWontAsk
  _ -> Extension
  where
    lower
      | BSC.any (\c -> c >= 'A' && c <= 'Z') bytes = BSC.map toLower bytes
      | otherwise = bytes
    request = Flag HeaderRequest
    response = Flag HeaderResponse


textGrammar :: ST.ShortText -> DirectiveGrammar
textGrammar text
  | SBS.length bytes == 1 = abbreviatedGrammar (SBSI.unsafeIndex bytes 0)
  | otherwise = directiveGrammar (ST.toByteString text)
  where
    bytes = ST.toShortByteString text
{-# INLINE textGrammar #-}


abbreviatedGrammar :: Word8 -> DirectiveGrammar
abbreviatedGrammar byte = case byte .|. 32 of
  119 -> Flag HeaderRequest MeterWillReportAndLimit -- w
  120 -> Flag HeaderRequest MeterWontReport -- x
  121 -> Flag HeaderRequest MeterWontLimit -- y
  99 -> Count -- c
  117 -> DecimalArgument MeterMaxUses -- u
  114 -> DecimalArgument MeterMaxReuses -- r
  100 -> Flag HeaderResponse MeterDoReport -- d
  101 -> Flag HeaderResponse MeterDontReport -- e
  116 -> DecimalArgument MeterTimeoutMinutes -- t
  110 -> Flag HeaderResponse MeterWontAsk -- n
  _ -> Extension
{-# INLINE abbreviatedGrammar #-}


{- | Interpret either spelling of a recognized directive. Invalid arguments
supplied through the raw constructor yield 'Nothing'.
-}
meterDirectiveKind :: MeterDirective -> Maybe MeterDirectiveKind
meterDirectiveKind (MeterDirective name value) =
  case directiveGrammar (ST.toByteString name) of
    Flag _ kind | Nothing <- value -> Just kind
    Count -> do
      (uses, suffix) <- BSC.readNatural =<< (ST.toByteString <$> value)
      ('/', rest) <- BSC.uncons suffix
      reuses <- wholeNatural rest
      pure (MeterCount (MeterUsageCounts uses reuses))
    DecimalArgument wrap -> wrap <$> (wholeNatural =<< (ST.toByteString <$> value))
    Extension -> Just (MeterExtension name value)
    _ -> Nothing
  where
    wholeNatural bytes = do
      (number, rest) <- BSC.readNatural bytes
      if BS.null rest then Just number else Nothing


-- | Construct a directive using the abbreviated RFC spelling.
meterDirectiveFromKind :: MeterDirectiveKind -> MeterDirective
meterDirectiveFromKind = \case
  MeterWillReportAndLimit -> flag "w"
  MeterWontReport -> flag "x"
  MeterWontLimit -> flag "y"
  MeterCount (MeterUsageCounts uses reuses) -> valued "c" (decimal uses <> M.char7 '/' <> decimal reuses)
  MeterMaxUses n -> valued "u" (decimal n)
  MeterMaxReuses n -> valued "r" (decimal n)
  MeterDoReport -> flag "d"
  MeterDontReport -> flag "e"
  MeterTimeoutMinutes n -> valued "t" (decimal n)
  MeterWontAsk -> flag "n"
  MeterExtension name value -> MeterDirective name value
  where
    flag name = MeterDirective name Nothing
    valued name = MeterDirective name . Just . STU.fromByteStringUnsafe . M.toStrictByteString
    decimal = M.integerDec . toInteger


renderMeter :: Meter -> M.Builder
renderMeter (Meter directives) = case directives of
  [] -> mempty
  first : rest -> renderDirective first <> foldMap ((", " <>) . renderDirective) rest
{-# INLINE renderMeter #-}


renderDirective :: MeterDirective -> M.Builder
renderDirective (MeterDirective name Nothing) = R.shortText name
renderDirective (MeterDirective name (Just value)) = R.shortText name <> M.char7 '=' <> argument
  where
    bytes = ST.toShortByteString value
    argument
      | not (SBS.null bytes) && SBS.all (isTokenChar . toEnum . fromIntegral) bytes = M.shortByteString bytes
      | Count <- directiveGrammar (ST.toByteString name)
      , SBS.all (\b -> (b >= 48 && b <= 57) || b == 47) bytes =
          M.shortByteString bytes
      | SBS.all (\b -> b == 9 || (b >= 32 && b < 127 && b /= 34 && b /= 92)) bytes =
          M.char7 '"' <> M.shortByteString bytes <> M.char7 '"'
      | otherwise = R.renderHttpQuotedString (ST.toText value)
{-# INLINE renderDirective #-}
