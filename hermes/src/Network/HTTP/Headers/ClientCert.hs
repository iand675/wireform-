{- |
RFC 9440 §2.2 @Client-Cert@ — request header inserted by a
TLS-terminating reverse proxy to convey the end-entity
certificate the client presented during mutual TLS to the
backend origin server.

== Grammar

@
Client-Cert = sf-binary
@

The value is a single
<https://www.rfc-editor.org/rfc/rfc9440.html#section-2.2 Structured Field>
Byte Sequence (RFC 8941 §3.3.5): the DER-encoded certificate,
base64-encoded and delimited by colons, e.g.

@
Client-Cert: :MIIBqDCCAU6gAwIBAgIBBzAK...:
@

The decoded DER bytes are surfaced verbatim; callers parse the
certificate with their own X.509 tooling.

Spec: <https://www.rfc-editor.org/rfc/rfc9440.html RFC 9440>.

See also: "Network.HTTP.Headers.ClientCertChain", "Network.HTTP.Headers.CertNotAfter", "Network.HTTP.Headers.CertNotBefore", "Network.HTTP.Headers.Forwarded".
-}
module Network.HTTP.Headers.ClientCert (
  ClientCert (..),
  ClientCertificateHeaders (..),
  lookupClientCertificateHeaders,
  clientCertParser,
  renderClientCert,
) where

import Data.ByteString (ByteString)
import Network.HTTP.Headers
import Network.HTTP.Headers.ClientCertChain (ClientCertChain (..))
import Network.HTTP.Headers.HeaderFieldName (hClientCert)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- | The DER-encoded end-entity certificate carried as a Byte Sequence.
newtype ClientCert = ClientCert {clientCertBytes :: ByteString}
  deriving stock (Eq, Show)


-- | The end-entity certificate and its optional accompanying chain.
data ClientCertificateHeaders = ClientCertificateHeaders
  { endEntityCertificate :: !ClientCert
  , accompanyingCertificateChain :: !(Maybe ClientCertChain)
  }
  deriving stock (Eq, Show)


-- | Read both fields and check the relationships defined in RFC 9440 section 2.3.
lookupClientCertificateHeaders :: HeaderMap -> Either String (Maybe ClientCertificateHeaders)
lookupClientCertificateHeaders fields = do
  leaf <- lookupHeader fields
  chain <- lookupHeader fields
  case (leaf, chain) of
    (Nothing, Nothing) -> Right Nothing
    (Nothing, Just _) -> Left "Client-Cert-Chain requires Client-Cert"
    (Just certificate, Just (ClientCertChain ancestors))
      | clientCertBytes certificate `elem` ancestors ->
          Left "Client-Cert-Chain contains the end-entity certificate"
    (Just certificate, _) -> Right (Just (ClientCertificateHeaders certificate chain))


instance KnownHeader ClientCert where
  type ParseFailure ClientCert = String
  type Cardinality ClientCert = 'ZeroOrOne
  type Direction ClientCert = 'Request


  parseFromHeaders _ = parseSingleFieldLine "Client-Cert" clientCertParser


  renderToHeaders _ = M.toStrictByteString . renderClientCert


  headerName _ = hClientCert


clientCertParser :: ParserT st String ClientCert
clientCertParser = ClientCert <$> rfc8941Binary


renderClientCert :: ClientCert -> M.Builder
renderClientCert (ClientCert bs) = R.rfc8941Binary bs
