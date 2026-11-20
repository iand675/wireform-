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

import Control.Monad (unless)
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as SBS
import Data.Char (chr)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as Text
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Data.Word (Word16, Word8)
import Network.HTTP.Grammar.Authority (portNumber, uriHost)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAltSvc)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R
import qualified Wireform.Builder as Builder


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


  parseFromHeaders _ = runAltSvcParser . combineFieldLines ", "


  renderToHeaders _ = pure . M.toStrictByteString . renderAltSvc


  headerName _ = hAltSvc


{- | Run 'altSvcParser' over the combined field value, rejecting
trailing junk (optional whitespace excepted).
-}
runAltSvcParser :: ByteString -> Either String AltSvc
runAltSvcParser = parseFieldLine "Alt-Svc" altSvcParser


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

altSvcParser :: ParserT st String AltSvc
altSvcParser =
  maybe AltSvcClear AltSvcValues . sequenceA
    <$> httpList1 "Alt-Svc" ((Just <$> altValueParser) <|> (Nothing <$ $(string "clear")))


altValueParser :: ParserT st String AltValue
altValueParser = do
  protocol <- rfc9110TokenBS
  $(char '=')
  unless (canonicalProtocolId protocol) (err "Non-canonical Alt-Svc protocol identifier")
  auth <- altAuthorityParser
  params <- many (ows *> $(char ';') *> ows *> altParameterParser)
  pure AltValue {altProtocolId = STU.fromByteStringUnsafe protocol, altAuthority = auth, altParameters = params}


canonicalProtocolId :: ByteString -> Bool
canonicalProtocolId bytes = case B.elemIndex 37 bytes of
  Nothing -> True
  Just offset
    | B.length bytes - offset >= 3
    , Just high <- uppercaseHex (B.index bytes (offset + 1))
    , Just low <- uppercaseHex (B.index bytes (offset + 2)) ->
        let octet = high `shiftL` 4 .|. low
        in (octet == 37 || not (isTokenChar (chr (fromIntegral octet))))
             && canonicalProtocolId (B.drop (offset + 3) bytes)
  _ -> False
  where
    uppercaseHex :: Word8 -> Maybe Word8
    uppercaseHex w
      | w >= 48 && w <= 57 = Just (w - 48)
      | w >= 65 && w <= 70 = Just (w - 55)
      | otherwise = Nothing


altAuthorityParser :: ParserT st String AltAuthority
altAuthorityParser = unescaped <|> withQuotedPairs
  where
    unescaped = $(char '"') *> authority <* $(char '"')
    authority = AltAuthority <$> optional uriHost <*> ($(char ':') *> portNumber)
    withQuotedPairs = do
      decoded <- httpQuotedBytes
      case runParser authority decoded of
        OK value "" -> pure value
        Err e -> err e
        _ -> failed


altParameterParser :: ParserT st String AltParameter
altParameterParser = do
  name <- rfc9110Token
  $(char '=')
  val <- (AltParamToken <$> rfc9110Token) <|> (AltParamQuoted . shortTextFromLatin1 <$> httpQuotedBytes)
  pure AltParameter {altParamName = name, altParamValue = val}


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

renderAltSvc :: AltSvc -> M.Builder
renderAltSvc AltSvcClear = "clear"
renderAltSvc (AltSvcValues (first :| rest)) =
  renderAltValue first <> foldMap (\value -> ", " <> renderAltValue value) rest


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
    renderVal (AltParamQuoted t)
      | SBS.any (>= 128) (ST.toShortByteString t) =
          M.char7 '"' <> Text.foldr (\c rest -> opaqueChar c <> rest) mempty (ST.toText t) <> M.char7 '"'
      | otherwise = R.rfc8941String (RFC8941String t)
    opaqueChar c
      | c == '"' || c == '\\' = M.char7 '\\' <> M.char7 c
      | c <= '\xff' = M.char8 c
      | otherwise = Builder.charUtf8 c


renderAltAuthority :: AltAuthority -> M.Builder
renderAltAuthority (AltAuthority mHost port) =
  M.char7 '"'
    <> maybe mempty R.shortText mHost
    <> M.char7 ':'
    <> M.word16Dec port
    <> M.char7 '"'
