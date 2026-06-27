{-# LANGUAGE TemplateHaskell #-}

{- |
@Public-Key-Pins@ (HPKP) — an /obsolete/ response header by which a host
pinned the set of public keys (as base64-encoded SPKI fingerprints) that
clients should expect to see in its certificate chain, for a duration of
@max-age@ seconds. HPKP was deprecated and is no longer honoured by major
browsers, but the wire grammar is well specified, so this module captures it
structurally.

== Grammar

@
Public-Key-Pins = pkp-d
pkp-d           = directive *( OWS ";" OWS directive )
directive       = simple-directive / pin-directive
simple-directive = directive-name [ "=" directive-value ]
pin-directive   = "pin-" token "=" quoted-string
directive-name  = token
directive-value = token / quoted-string
@

The defined directives are the pin set @pin-\<hash-alg\>="\<fingerprint\>"@,
@max-age=\<delta-seconds\>@ (REQUIRED), and the valueless flag
@includeSubDomains@, plus the optional @report-uri="\<uri\>"@. Directive names
are case-insensitive; the first occurrence of @max-age@ / @report-uri@ wins and
unrecognised directives are ignored (RFC 7469 §2.1).

Spec: <https://www.rfc-editor.org/rfc/rfc7469#section-2.1>

See also: "Network.HTTP.Headers.PublicKeyPinsReportOnly", "Network.HTTP.Headers.StrictTransportSecurity", "Network.HTTP.Headers.ExpectCT".
-}
module Network.HTTP.Headers.PublicKeyPins (
  PublicKeyPins (..),
  Pin (..),
  publicKeyPinsParser,
  renderPublicKeyPins,
) where

import qualified Control.Applicative
import Control.Monad (void)
import qualified Data.ByteString as B
import Data.Char (toLower)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hPublicKeyPins)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


{- | A single pinned public key: a hash algorithm and a base64-encoded SPKI
fingerprint, as carried by a @pin-\<hash-alg\>="\<fingerprint\>"@ directive.
-}
data Pin = Pin
  { pinAlgorithm :: !ST.ShortText
  -- ^ The hash algorithm name following the @pin-@ prefix (e.g. @sha256@).
  , pinFingerprint :: !ST.ShortText
  -- ^ The base64-encoded Subject Public Key Info fingerprint.
  }
  deriving stock (Eq, Show)


-- | Parsed @Public-Key-Pins@ policy (RFC 7469 §2.1).
data PublicKeyPins = PublicKeyPins
  { pkpPins :: ![Pin]
  -- ^ The pinned public keys, in the order they appeared.
  , pkpMaxAge :: !Word
  -- ^ @max-age@ delta-seconds: how long the pins are to be enforced.
  , pkpIncludeSubDomains :: !Bool
  -- ^ Whether the policy also applies to every subdomain of the host.
  , pkpReportUri :: !(Maybe ST.ShortText)
  -- ^ Optional URI to which pin-validation failures should be reported.
  }
  deriving stock (Eq, Show)


instance KnownHeader PublicKeyPins where
  type ParseFailure PublicKeyPins = String
  type Cardinality PublicKeyPins = 'ZeroOrOne
  type Direction PublicKeyPins = 'Response


  parseFromHeaders _ headers = case runParser publicKeyPinsParser (NE.head headers) of
    OK pkp leftover
      | B.null (dropOws leftover) -> Right pkp
      | otherwise ->
          Left ("Unconsumed input after parsing Public-Key-Pins: " <> show leftover)
    Fail -> Left "Failed to parse Public-Key-Pins header"
    Err err -> Left err
    where
      dropOws = B.dropWhile (\w -> w == 0x20 || w == 0x09)


  renderToHeaders _ = M.toStrictByteString . renderPublicKeyPins


  headerName _ = hPublicKeyPins


-- | One parsed directive (internal).
data Directive
  = DPin !Pin
  | DMaxAge !Word
  | DIncludeSubDomains
  | DReportUri !ST.ShortText
  | DOther


publicKeyPinsParser :: ParserT st String PublicKeyPins
publicKeyPinsParser = do
  ows
  first <- directive
  rest <- many (ows *> $(char ';') *> ows *> directive)
  ows
  maybe failed pure (applyDirectives (first : rest))
  where
    directive = do
      name <- rfc9110Token
      let raw = ST.toString name
          lname = map toLower raw
      if take 4 lname == "pin-"
        then do
          v <- ows *> $(char '=') *> ows *> quotedString
          pure (DPin (Pin (ST.fromString (drop 4 raw)) v))
        else case lname of
          "max-age" -> DMaxAge <$> (ows *> $(char '=') *> ows *> maxAgeValue)
          "includesubdomains" -> pure DIncludeSubDomains
          "report-uri" -> DReportUri <$> (ows *> $(char '=') *> ows *> quotedString)
          _ -> DOther <$ optional (ows *> $(char '=') *> ows *> directiveValue)
    maxAgeValue =
      ($(char '"') *> anyAsciiDecimalWord <* $(char '"'))
        <|> anyAsciiDecimalWord
    directiveValue = void quotedString <|> void rfc9110Token


{- | Fold parsed directives into the structured record. @max-age@ is required
(first occurrence wins); pins accumulate in order; the first @report-uri@
wins.
-}
applyDirectives :: [Directive] -> Maybe PublicKeyPins
applyDirectives = go [] Nothing False Nothing
  where
    go pins (Just ma) inc rpt [] = Just (PublicKeyPins (reverse pins) ma inc rpt)
    go _ Nothing _ _ [] = Nothing
    go pins ma inc rpt (d : ds) = case d of
      DPin p -> go (p : pins) ma inc rpt ds
      DMaxAge n -> go pins (ma Control.Applicative.<|> Just n) inc rpt ds
      DIncludeSubDomains -> go pins ma True rpt ds
      DReportUri u -> go pins ma inc (rpt Control.Applicative.<|> Just u) ds
      DOther -> go pins ma inc rpt ds


renderPublicKeyPins :: PublicKeyPins -> M.Builder
renderPublicKeyPins (PublicKeyPins pins ma inc rpt) =
  M.intersperse "; " parts
  where
    parts =
      map renderPin pins
        ++ ["max-age=" <> M.wordDec ma]
        ++ ["includeSubDomains" | inc]
        ++ maybe [] (\u -> ["report-uri=" <> quoted u]) rpt
    renderPin (Pin alg fp) = "pin-" <> shortText alg <> "=" <> quoted fp


{- | Render a value as an HTTP @quoted-string@ (no escaping; pin fingerprints
and report URIs do not contain @\"@ or @\\@).
-}
quoted :: ST.ShortText -> M.Builder
quoted t = M.char7 '"' <> shortText t <> M.char7 '"'
