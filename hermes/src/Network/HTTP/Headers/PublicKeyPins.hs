{- |
@Public-Key-Pins@ (HPKP) — an /obsolete/ response header by which a host
pinned the set of public keys (as base64-encoded SPKI fingerprints) that
clients should expect to see in its certificate chain, for a duration of
@max-age@ seconds. HPKP was deprecated and is no longer honoured by major
browsers, but the wire grammar is well specified, so this module captures it
structurally.

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

The @max-age@ directive is required.

Spec: <https://www.rfc-editor.org/rfc/rfc7469#section-2.1>

See also: "Network.HTTP.Headers.PublicKeyPinsReportOnly", "Network.HTTP.Headers.StrictTransportSecurity", "Network.HTTP.Headers.ExpectCT".
-}
module Network.HTTP.Headers.PublicKeyPins (
  PublicKeyPins (..),
  Pin (..),
  publicKeyPinsParser,
  renderPublicKeyPins,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hPublicKeyPins)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.TransportPolicy (encodePinPolicyChecked, pinPolicy, renderPinPolicy)


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


  -- RFC 7469 section 2.3.1 uses the first occurrence of each field.
  parseFromHeaders _ = parseFieldLine "Public-Key-Pins" publicKeyPinsParser . NE.head


  renderToHeaders _ = M.toStrictByteString . renderPublicKeyPins


  renderToHeadersChecked _ (PublicKeyPins pins age subdomains uri) =
    encodePinPolicyChecked pinAlgorithm pinFingerprint pins (Just age) subdomains uri


  headerName _ = hPublicKeyPins


publicKeyPinsParser :: ParserT st String PublicKeyPins
publicKeyPinsParser = pinPolicy Pin PublicKeyPins Nothing


renderPublicKeyPins :: PublicKeyPins -> M.Builder
renderPublicKeyPins (PublicKeyPins pins age subdomains uri) =
  renderPinPolicy pinAlgorithm pinFingerprint pins (Just age) subdomains uri
