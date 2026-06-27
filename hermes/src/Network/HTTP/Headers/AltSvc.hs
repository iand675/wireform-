{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 7838 §3 @Alt-Svc@ — the response header by which an origin
advertises that its resources are also available, possibly over a
different protocol or authority, at one or more /alternative
services/.

== Grammar (RFC 7838 §3)

@
Alt-Svc       = clear / 1#alt-value
clear         = %s\"clear\"
alt-value     = alternative *( OWS \";\" OWS parameter )
alternative   = protocol-id \"=\" alt-authority
protocol-id   = token              ; percent-encoded ALPN protocol name
alt-authority = quoted-string      ; containing [ uri-host ] \":\" port
parameter     = token \"=\" ( token / quoted-string )
@

The literal token @clear@ asks the client to invalidate every
cached alternative for the origin.  Otherwise the value is a
comma-separated list of alternatives, each binding an ALPN
@protocol-id@ to an @alt-authority@ (@host:port@, host optional ==
\"same as origin\") plus optional parameters.  The two
IANA-registered parameters are @ma@ (max-age, delta-seconds) and
@persist@; this module keeps the parameter list generic and in
document order so any current or future parameter round-trips.

Spec: <https://www.rfc-editor.org/rfc/rfc7838#section-3>

See also: "Network.HTTP.Headers.AltUsed", "Network.HTTP.Headers.ALPN", "Network.HTTP.Headers.Upgrade".
-}
module Network.HTTP.Headers.AltSvc (
  -- * Types
  AltSvc (..),
  AltValue (..),
  AltAuthority (..),
  AltParameter (..),
  AltParamValue (..),

  -- * Parsing
  altSvcParser,
  altValueParser,
  altAuthorityParser,
  altParameterParser,
  runAltSvcParser,

  -- * Rendering
  renderAltSvc,
  renderAltValue,
) where

import qualified Control.Monad.Combinators.NonEmpty as NE1
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.CharSet (CharSet)
import qualified Data.CharSet as CharSet
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text.Short (ShortText)
import Data.Word (Word16)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAltSvc)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- ---------------------------------------------------------------------------
-- Data
-- ---------------------------------------------------------------------------

{- | A parsed @Alt-Svc@ value: either the @clear@ sentinel or one
or more alternatives in document order.
-}
data AltSvc
  = -- | The literal @clear@ — invalidate all cached alternatives.
    AltSvcClear
  | -- | A non-empty list of advertised alternatives.
    AltSvcValues !(NonEmpty AltValue)
  deriving stock (Eq, Show)


-- | @alternative *( OWS \";\" OWS parameter )@.
data AltValue = AltValue
  { altProtocolId :: !ShortText
  -- ^ The (percent-encoded) ALPN @protocol-id@, e.g. @h2@, @h3@.
  , altAuthority :: !AltAuthority
  -- ^ The @host:port@ the alternative is reachable at.
  , altParameters :: ![AltParameter]
  -- ^ Optional parameters (@ma@, @persist@, …) in document order.
  }
  deriving stock (Eq, Show)


{- | @alt-authority@: a quoted @[ uri-host ] \":\" port@.  A missing
host means \"the same host as the origin\".  The host is stored
verbatim (including bracketed IPv6 literals such as @[::1]@).
-}
data AltAuthority = AltAuthority
  { altAuthHost :: !(Maybe ShortText)
  , altAuthPort :: !Word16
  }
  deriving stock (Eq, Show)


-- | A single @parameter@ — @token \"=\" ( token / quoted-string )@.
data AltParameter = AltParameter
  { altParamName :: !ShortText
  , altParamValue :: !AltParamValue
  }
  deriving stock (Eq, Show)


{- | A parameter value, preserving whether it appeared as a bare
@token@ or a @quoted-string@ so rendering round-trips.
-}
data AltParamValue
  = AltParamToken !ShortText
  | AltParamQuoted !ShortText
  deriving stock (Eq, Show)


instance KnownHeader AltSvc where
  type ParseFailure AltSvc = String
  type Cardinality AltSvc = 'ZeroOrMore
  type Direction AltSvc = 'Response


  parseFromHeaders _ headers = combineAltSvc <$> traverse runAltSvcParser headers


  renderToHeaders _ = pure . M.toStrictByteString . renderAltSvc


  headerName _ = hAltSvc


{- | Combine the per-line parses of a multi-line @Alt-Svc@ header.
@clear@ is absorbing (it invalidates everything); otherwise the
alternative lists are concatenated in order.
-}
combineAltSvc :: NonEmpty AltSvc -> AltSvc
combineAltSvc (x :| xs) = foldl step x xs
  where
    step AltSvcClear _ = AltSvcClear
    step _ AltSvcClear = AltSvcClear
    step (AltSvcValues a) (AltSvcValues b) = AltSvcValues (a <> b)


{- | Run 'altSvcParser' over one physical header line, rejecting
trailing junk (optional whitespace excepted).
-}
runAltSvcParser :: ByteString -> Either String AltSvc
runAltSvcParser bs = case runParser altSvcParser bs of
  OK v leftover
    | B.null (B.dropWhile isOwsByte leftover) -> Right v
    | otherwise -> Left ("Unconsumed input after parsing Alt-Svc header: " <> show leftover)
  Fail -> Left "Failed to parse Alt-Svc header"
  Err e -> Left e
  where
    isOwsByte w = w == 0x20 || w == 0x09


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

altSvcParser :: ParserT st String AltSvc
altSvcParser = do
  ows
  (AltSvcValues <$> (altValueParser `NE1.sepBy1` (ows *> $(char ',') *> ows)))
    <|> (AltSvcClear <$ $(string "clear"))


altValueParser :: ParserT st String AltValue
altValueParser = do
  proto <- rfc9110Token
  $(char '=')
  auth <- altAuthorityParser
  params <- many (ows *> $(char ';') *> ows *> altParameterParser)
  pure AltValue {altProtocolId = proto, altAuthority = auth, altParameters = params}


altAuthorityParser :: ParserT st String AltAuthority
altAuthorityParser = do
  $(char '"')
  mHost <- optional hostParser
  $(char ':')
  port <- anyAsciiDecimalWord
  $(char '"')
  pure AltAuthority {altAuthHost = mHost, altAuthPort = fromIntegral port}


altParameterParser :: ParserT st String AltParameter
altParameterParser = do
  name <- rfc9110Token
  ows
  $(char '=')
  ows
  val <- (AltParamToken <$> rfc9110Token) <|> (AltParamQuoted <$> quotedString)
  pure AltParameter {altParamName = name, altParamValue = val}


{- | @uri-host@: a bracketed IP-literal (e.g. @[::1]@, captured with
its brackets) or a @reg-name@ \/ IPv4address (no colon).
-}
hostParser :: ParserT st e ShortText
hostParser = bracketed <|> regName
  where
    bracketed = shortASCIIFromParser_ $ do
      $(char '[')
      skipSome (skipSatisfyAscii (/= ']'))
      $(char ']')
    regName =
      shortASCIIFromParser_ $
        skipSome (skipSatisfyAscii (`CharSet.member` hostCharSet))


{- | Characters permitted in a non-bracketed @uri-host@: @unreserved@
plus @sub-delims@ (minus @,@) plus @%@ for pct-encoding.  Excludes
@:@, @\"@, @[@ and @]@ so parsing stops at the port separator and
closing quote.
-}
hostCharSet :: CharSet
hostCharSet =
  CharSet.range 'A' 'Z'
    <> CharSet.range 'a' 'z'
    <> CharSet.range '0' '9'
    <> "-._~%!$&'()*+;="


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

renderAltSvc :: AltSvc -> M.Builder
renderAltSvc AltSvcClear = "clear"
renderAltSvc (AltSvcValues vs) = R.sepByCommas1 (fmap renderAltValue vs)


renderAltValue :: AltValue -> M.Builder
renderAltValue (AltValue proto auth params) =
  R.shortText proto
    <> M.char7 '='
    <> renderAltAuthority auth
    <> foldMap renderParam params
  where
    renderParam (AltParameter name val) =
      "; " <> R.shortText name <> M.char7 '=' <> renderVal val
    renderVal (AltParamToken t) = R.shortText t
    renderVal (AltParamQuoted t) = R.rfc8941String (RFC8941String t)


renderAltAuthority :: AltAuthority -> M.Builder
renderAltAuthority (AltAuthority mHost port) =
  M.char7 '"'
    <> maybe mempty R.shortText mHost
    <> M.char7 ':'
    <> M.word16Dec port
    <> M.char7 '"'
