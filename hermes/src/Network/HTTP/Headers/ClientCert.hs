{-# LANGUAGE TemplateHaskell #-}

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
  clientCertParser,
  renderClientCert,
) where

import Data.ByteString (ByteString)
import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hClientCert)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- | The DER-encoded end-entity certificate carried as a Byte Sequence.
newtype ClientCert = ClientCert {clientCertBytes :: ByteString}
  deriving stock (Eq, Show)


instance KnownHeader ClientCert where
  type ParseFailure ClientCert = String
  type Cardinality ClientCert = 'ZeroOrOne
  type Direction ClientCert = 'Request


  parseFromHeaders _ headers = case runParser clientCertParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Client-Cert header: " <> show rest
    Fail -> Left "Failed to parse Client-Cert header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderClientCert


  headerName _ = hClientCert


clientCertParser :: ParserT st String ClientCert
clientCertParser = ClientCert <$> (rfc8941Binary <* $(char ':'))


renderClientCert :: ClientCert -> M.Builder
renderClientCert (ClientCert bs) = R.rfc8941Binary bs <> M.char7 ':'
