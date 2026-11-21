{- |
@Cert-Not-Before@ is an ACME STAR certificate response header. It
carries the start of the downloaded end-entity certificate's validity
period as a singleton HTTP-date.

@
Cert-Not-Before: Thu, 03 Oct 2019 00:00:00 GMT
@

Spec: <https://www.rfc-editor.org/rfc/rfc8739.html#section-3.3>

See also: "Network.HTTP.Headers.CertNotAfter", "Network.HTTP.Headers.Date",
"Network.HTTP.Headers.Expires".
-}
module Network.HTTP.Headers.CertNotBefore (
  CertNotBefore (..),
  certNotBeforeParser,
  renderCertNotBefore,
) where

import Data.Time.Clock (UTCTime)
import Network.HTTP.Headers
import Network.HTTP.Headers.Date (dateParser, dateParserWith, renderDate, renderDateBytesChecked)
import Network.HTTP.Headers.HeaderFieldName (hCertNotBefore)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | The certificate's @notBefore@ timestamp.
newtype CertNotBefore = CertNotBefore {certNotBeforeTime :: UTCTime}
  deriving stock (Eq, Show)


instance KnownHeader CertNotBefore where
  type ParseFailure CertNotBefore = String
  type Cardinality CertNotBefore = 'ZeroOrOne
  type Direction CertNotBefore = 'Response


  parseFromHeaders settings = parseSingleFieldLine "Cert-Not-Before" (CertNotBefore <$> dateParserWith settings)


  renderToHeaders _ = M.toStrictByteString . renderCertNotBefore


  renderToHeadersChecked _ = renderDateBytesChecked . certNotBeforeTime


  headerName _ = hCertNotBefore


certNotBeforeParser :: ParserT st String CertNotBefore
certNotBeforeParser = CertNotBefore <$> dateParser


renderCertNotBefore :: CertNotBefore -> M.Builder
renderCertNotBefore (CertNotBefore time) = renderDate time
