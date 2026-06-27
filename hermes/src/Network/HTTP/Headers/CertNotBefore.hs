{-# LANGUAGE TemplateHaskell #-}

{- |
@Cert-Not-Before@ — companion request header to the RFC 9440
Client-Cert fields, inserted by a TLS-terminating reverse proxy to
convey the start of the validity window (@notBefore@) of the
client's end-entity certificate.

== Grammar

@
Cert-Not-Before = sf-date
@

The value is a
<https://www.rfc-editor.org/rfc/rfc9651.html#section-3.3.7 Structured Field>
Date (RFC 9651 §3.3.7): an integer number of seconds relative to the
Unix epoch (1970-01-01T00:00:00Z) prefixed with @\@@, e.g.

@
Cert-Not-Before: @1659578233
@

The value may be negative for instants before the epoch. We model it
as the raw signed second count so the round-trip is exact.

Spec: <https://www.rfc-editor.org/rfc/rfc9440.html RFC 9440>.

See also: "Network.HTTP.Headers.CertNotAfter", "Network.HTTP.Headers.ClientCert", "Network.HTTP.Headers.ClientCertChain", "Network.HTTP.Headers.Forwarded".
-}
module Network.HTTP.Headers.CertNotBefore (
  CertNotBefore (..),
  certNotBeforeParser,
  renderCertNotBefore,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hCertNotBefore)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | Certificate @notBefore@ instant as seconds since the Unix epoch.
newtype CertNotBefore = CertNotBefore {certNotBeforeSeconds :: Int}
  deriving stock (Eq, Show)


instance KnownHeader CertNotBefore where
  type ParseFailure CertNotBefore = String
  type Cardinality CertNotBefore = 'ZeroOrOne
  type Direction CertNotBefore = 'Request


  parseFromHeaders _ headers = case runParser certNotBeforeParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Cert-Not-Before header: " <> show rest
    Fail -> Left "Failed to parse Cert-Not-Before header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderCertNotBefore


  headerName _ = hCertNotBefore


certNotBeforeParser :: ParserT st String CertNotBefore
certNotBeforeParser = CertNotBefore <$> ($(char '@') *> rfc8941Integer)


renderCertNotBefore :: CertNotBefore -> M.Builder
renderCertNotBefore (CertNotBefore secs) = M.char7 '@' <> M.intDec secs
