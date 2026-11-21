{- |
@Cert-Not-After@ is an ACME STAR certificate response header. It
carries the end of the downloaded end-entity certificate's validity
period as a singleton HTTP-date.

@
Cert-Not-After: Thu, 10 Oct 2019 00:00:00 GMT
@

Spec: <https://www.rfc-editor.org/rfc/rfc8739.html#section-3.3>

See also: "Network.HTTP.Headers.CertNotBefore", "Network.HTTP.Headers.Date",
"Network.HTTP.Headers.Expires".
-}
module Network.HTTP.Headers.CertNotAfter (
  CertNotAfter (..),
  certNotAfterParser,
  renderCertNotAfter,
) where

import Data.Time.Clock (UTCTime)
import Network.HTTP.Headers
import Network.HTTP.Headers.Date (dateParser, dateParserWith, renderDate, renderDateBytesChecked)
import Network.HTTP.Headers.HeaderFieldName (hCertNotAfter)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | The certificate's @notAfter@ timestamp.
newtype CertNotAfter = CertNotAfter {certNotAfterTime :: UTCTime}
  deriving stock (Eq, Show)


instance KnownHeader CertNotAfter where
  type ParseFailure CertNotAfter = String
  type Cardinality CertNotAfter = 'ZeroOrOne
  type Direction CertNotAfter = 'Response


  parseFromHeaders settings = parseSingleFieldLine "Cert-Not-After" (CertNotAfter <$> dateParserWith settings)


  renderToHeaders _ = M.toStrictByteString . renderCertNotAfter


  renderToHeadersChecked _ = renderDateBytesChecked . certNotAfterTime


  headerName _ = hCertNotAfter


certNotAfterParser :: ParserT st String CertNotAfter
certNotAfterParser = CertNotAfter <$> dateParser


renderCertNotAfter :: CertNotAfter -> M.Builder
renderCertNotAfter (CertNotAfter time) = renderDate time
