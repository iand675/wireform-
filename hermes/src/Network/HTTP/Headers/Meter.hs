{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 2227 @Meter@ — HTTP metering directives exchanged between a proxy cache
and an origin server to report, and place limits on, reuse of a cached
response.

== Grammar

@
Meter           = \"Meter\" \":\" #meter-directive
meter-directive = meter-name [ \"=\" ( token | quoted-string ) ]
meter-name      = token
@

The registered @meter-name@s include @do-report@\\/@d@,
@will-report-and-limit@\\/@w@, @wont-report@\\/@x@, @wont-limit@\\/@y@,
@wont-ask@\\/@!@, @count@\\/@c@, @max-uses@\\/@u@ and @max-refuses@\\/@r@.
Rather than enumerate the (rarely deployed) registry, every directive is
surfaced as a flat name with an optional value. A value is rendered as a
bare token when it is token-safe and as a quoted-string (escaping @\"@ and
@\\@) otherwise.

Spec: <https://www.rfc-editor.org/rfc/rfc2227>

See also: "Network.HTTP.Headers.CacheControl", "Network.HTTP.Headers.Age", "Network.HTTP.Headers.Pragma", "Network.HTTP.Headers.Warning".
-}
module Network.HTTP.Headers.Meter (
  Meter (..),
  MeterDirective (..),
  meterParser,
  renderMeter,
) where

import qualified Data.ByteString as B
import qualified Data.CharSet as CharSet
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hMeter)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- | A single meter-directive: a token name, optionally with a value.
data MeterDirective = MeterDirective
  { meterName :: !ST.ShortText
  , meterValue :: !(Maybe ST.ShortText)
  }
  deriving stock (Eq, Show)


-- | The comma-separated list of directives carried by a @Meter@ header.
newtype Meter = Meter {meterDirectives :: [MeterDirective]}
  deriving stock (Eq, Show)


instance KnownHeader Meter where
  type ParseFailure Meter = String
  type Cardinality Meter = 'ZeroOrOne
  type Direction Meter = 'RequestAndResponse


  parseFromHeaders _ headers = case runParser meterParser (NE.head headers) of
    OK v leftover
      | B.null (dropOws leftover) -> Right v
      | otherwise -> Left ("Unconsumed input after parsing Meter: " <> show leftover)
    Fail -> Left "Failed to parse Meter header"
    Err err -> Left err
    where
      dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


  renderToHeaders _ = M.toStrictByteString . renderMeter


  headerName _ = hMeter


meterParser :: ParserT st String Meter
meterParser = Meter <$> (ows *> directives)
  where
    directives = nonEmpty <|> pure []
    nonEmpty = do
      d <- directive
      ds <- many (ows *> $(char ',') *> ows *> directive)
      pure (d : ds)
    directive = do
      name <- rfc9110Token
      val <- optional ($(char '=') *> (rfc9110Token <|> quotedString))
      pure (MeterDirective name val)


renderMeter :: Meter -> M.Builder
renderMeter (Meter ds) = M.intersperse ", " (map renderDirective ds)
  where
    renderDirective (MeterDirective name Nothing) = R.shortText name
    renderDirective (MeterDirective name (Just v)) =
      R.shortText name <> M.char7 '=' <> renderTokenOrQuoted v


{- | Render a value as a bare token when it is token-safe, otherwise as a
quoted-string with the mandatory escaping of @\"@ and @\\@.
-}
renderTokenOrQuoted :: ST.ShortText -> M.Builder
renderTokenOrQuoted t
  | isToken s = R.shortText t
  | otherwise = M.char8 '"' <> foldr esc mempty s <> M.char8 '"'
  where
    s = ST.toString t
    isToken cs = not (null cs) && all (`CharSet.member` tokenCharSet) cs
    esc c acc
      | c == '"' || c == '\\' = M.char8 '\\' <> M.char8 c <> acc
      | otherwise = M.char8 c <> acc
