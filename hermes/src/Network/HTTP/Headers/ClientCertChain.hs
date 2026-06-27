{-# LANGUAGE TemplateHaskell #-}

{- |
RFC 9440 §2.3 @Client-Cert-Chain@ — request header inserted by a
TLS-terminating reverse proxy to convey the certificate chain
(intermediate CA certificates) that accompanied the client's
end-entity certificate during mutual TLS.

== Grammar

@
Client-Cert-Chain = sf-list
@

The value is a
<https://www.rfc-editor.org/rfc/rfc9440.html#section-2.3 Structured Field>
List (RFC 8941 §3.1) of Byte Sequences, each a DER-encoded
certificate, base64-encoded and colon-delimited, in order from the
certificate closest to the end-entity certificate to the trust
anchor, e.g.

@
Client-Cert-Chain: :MIIBxzCC...:, :MIIB...:
@

Because it is a List, multiple physical header lines are combined
into one logical value. Each member's decoded DER bytes are
surfaced verbatim.

Spec: <https://www.rfc-editor.org/rfc/rfc9440.html RFC 9440>.

See also: "Network.HTTP.Headers.ClientCert", "Network.HTTP.Headers.CertNotAfter", "Network.HTTP.Headers.CertNotBefore", "Network.HTTP.Headers.Forwarded".
-}
module Network.HTTP.Headers.ClientCertChain (
  ClientCertChain (..),
  clientCertChainParser,
  renderClientCertChain,
) where

import Data.ByteString (ByteString)
import Data.Foldable1 (fold1)
import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hClientCertChain)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import qualified Network.HTTP.Headers.Rendering.Util as R


-- | An ordered, non-empty list of DER-encoded chain certificates.
newtype ClientCertChain = ClientCertChain {clientCertChain :: NE.NonEmpty ByteString}
  deriving stock (Eq, Show)
  deriving newtype (Semigroup)


instance KnownHeader ClientCertChain where
  type ParseFailure ClientCertChain = String
  type Cardinality ClientCertChain = 'ZeroOrMore
  type Direction ClientCertChain = 'Request


  parseFromHeaders _ headers = do
    res <- traverse runChainParser headers
    pure $ fold1 res


  renderToHeaders _ = pure . M.toStrictByteString . renderClientCertChain


  headerName _ = hClientCertChain


runChainParser :: ByteString -> Either String ClientCertChain
runChainParser bs = case runParser clientCertChainParser bs of
  OK v "" -> Right v
  OK _ rest -> Left $ "Unconsumed input after parsing Client-Cert-Chain header: " <> show rest
  Fail -> Left "Failed to parse Client-Cert-Chain header"
  Err e -> Left e


clientCertChainParser :: ParserT st String ClientCertChain
clientCertChainParser = ClientCertChain <$> rfc8941List1 (rfc8941Binary <* $(char ':'))


renderClientCertChain :: ClientCertChain -> M.Builder
renderClientCertChain (ClientCertChain chain) =
  R.sepByCommas1 (fmap renderCert chain)
  where
    renderCert bs = R.rfc8941Binary bs <> M.char7 ':'
