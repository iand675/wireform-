{-# LANGUAGE TemplateHaskell #-}

{- |
@Cert-Not-After@ — companion request header to the RFC 9440
Client-Cert fields, inserted by a TLS-terminating reverse proxy to
convey the end of the validity window (@notAfter@) of the client's
end-entity certificate.

== Grammar

@
Cert-Not-After = sf-date
@

The value is a
<https://www.rfc-editor.org/rfc/rfc9651.html#section-3.3.7 Structured Field>
Date (RFC 9651 §3.3.7): an integer number of seconds relative to the
Unix epoch (1970-01-01T00:00:00Z) prefixed with @\@@, e.g.

@
Cert-Not-After: @1690354800
@

The value may be negative for instants before the epoch. We model it
as the raw signed second count so the round-trip is exact.

Spec: <https://www.rfc-editor.org/rfc/rfc9440.html RFC 9440>.

See also: "Network.HTTP.Headers.CertNotBefore", "Network.HTTP.Headers.ClientCert", "Network.HTTP.Headers.ClientCertChain", "Network.HTTP.Headers.Forwarded".
-}
module Network.HTTP.Headers.CertNotAfter (
  CertNotAfter (..),
  certNotAfterParser,
  renderCertNotAfter,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hCertNotAfter)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | Certificate @notAfter@ instant as seconds since the Unix epoch.
newtype CertNotAfter = CertNotAfter {certNotAfterSeconds :: Int}
  deriving stock (Eq, Show)


instance KnownHeader CertNotAfter where
  type ParseFailure CertNotAfter = String
  type Cardinality CertNotAfter = 'ZeroOrOne
  type Direction CertNotAfter = 'Request


  parseFromHeaders _ headers = case runParser certNotAfterParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Cert-Not-After header: " <> show rest
    Fail -> Left "Failed to parse Cert-Not-After header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderCertNotAfter


  headerName _ = hCertNotAfter


certNotAfterParser :: ParserT st String CertNotAfter
certNotAfterParser = CertNotAfter <$> ($(char '@') *> rfc8941Integer)


renderCertNotAfter :: CertNotAfter -> M.Builder
renderCertNotAfter (CertNotAfter secs) = M.char7 '@' <> M.intDec secs
