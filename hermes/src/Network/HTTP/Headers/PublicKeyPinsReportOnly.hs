{- |
@Public-Key-Pins-Report-Only@ (HPKP) — an /obsolete/ response header with the
same syntax as @Public-Key-Pins@, but which only reports pin-validation
failures (via @report-uri@) without actually enforcing the pins. HPKP was
deprecated and is no longer honoured by major browsers, but the wire grammar is
well specified, so this module captures it structurally.

== Grammar

@
Public-Key-Directives = directive *( OWS ";" OWS directive )
directive             = directive-name [ "=" directive-value ]
directive-name        = token
directive-value       = token / quoted-string
@

The repeated @pin-sha256@ directive takes a quoted Base64 fingerprint.
Other directives are unique and case-insensitive. Unknown directives and
pins for unsupported hash algorithms are ignored.

The optional @max-age@ directive is retained for wire compatibility and has
no effect on a report-only policy.

Spec: <https://www.rfc-editor.org/rfc/rfc7469#section-2.1>

See also: "Network.HTTP.Headers.PublicKeyPins", "Network.HTTP.Headers.StrictTransportSecurity", "Network.HTTP.Headers.ExpectCT".
-}
module Network.HTTP.Headers.PublicKeyPinsReportOnly (
  PublicKeyPinsReportOnly (..),
  Pin (..),
  publicKeyPinsReportOnlyParser,
  renderPublicKeyPinsReportOnly,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hPublicKeyPinsReportOnly)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.TransportPolicy (pinPolicy, renderPinPolicy)


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


-- | Parsed @Public-Key-Pins-Report-Only@ policy (RFC 7469 §2.1).
data PublicKeyPinsReportOnly = PublicKeyPinsReportOnly
  { pkproPins :: ![Pin]
  -- ^ The pinned public keys, in the order they appeared.
  , pkproMaxAge :: !Word
  -- ^ @max-age@ delta-seconds for wire compatibility. An omitted value reads as zero.
  , pkproIncludeSubDomains :: !Bool
  -- ^ Whether the policy also applies to every subdomain of the host.
  , pkproReportUri :: !(Maybe ST.ShortText)
  -- ^ Optional URI to which pin-validation failures should be reported.
  }
  deriving stock (Eq, Show)


instance KnownHeader PublicKeyPinsReportOnly where
  type ParseFailure PublicKeyPinsReportOnly = String
  type Cardinality PublicKeyPinsReportOnly = 'ZeroOrOne
  type Direction PublicKeyPinsReportOnly = 'Response


  -- RFC 7469 section 2.3.1 uses the first occurrence of each field.
  parseFromHeaders _ = parseFieldLine "Public-Key-Pins-Report-Only" publicKeyPinsReportOnlyParser . NE.head


  renderToHeaders _ = M.toStrictByteString . renderPublicKeyPinsReportOnly


  headerName _ = hPublicKeyPinsReportOnly


publicKeyPinsReportOnlyParser :: ParserT st String PublicKeyPinsReportOnly
publicKeyPinsReportOnlyParser = pinPolicy Pin PublicKeyPinsReportOnly (Just 0)


renderPublicKeyPinsReportOnly :: PublicKeyPinsReportOnly -> M.Builder
renderPublicKeyPinsReportOnly (PublicKeyPinsReportOnly pins age subdomains uri) =
  renderPinPolicy pinAlgorithm pinFingerprint pins ageDirective subdomains uri
  where
    ageDirective
      | age == 0 && (not (null pins) || subdomains || uri /= Nothing) = Nothing
      | otherwise = Just age
