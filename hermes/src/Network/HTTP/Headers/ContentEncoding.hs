{- |
@Content-Encoding@ lists the content codings (e.g. @gzip@, @br@, @zstd@) that
have been applied to the representation on top of its media type, in the order
they were applied; the recipient reverses them to recover the original data. It
is representation metadata that may appear on both requests and responses, and
is the counterpart to the @Accept-Encoding@ negotiation header.

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-8.4>

See also: "Network.HTTP.Headers.ContentType", "Network.HTTP.Headers.ContentLanguage", "Network.HTTP.Headers.AcceptEncoding", "Network.HTTP.Headers.TransferEncoding".
-}
module Network.HTTP.Headers.ContentEncoding where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.ContentCoding
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hContentEncoding)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


newtype ContentEncoding = ContentEncoding
  { contentEncoding :: ContentCoding
  }
  deriving stock (Eq, Show)


instance KnownHeader ContentEncoding where
  type ParseFailure ContentEncoding = String
  type Cardinality ContentEncoding = 'ZeroOrOne
  type Direction ContentEncoding = 'Response


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser contentEncodingParser header of
      OK ce "" -> Right ce
      OK _ rest -> Left $ "Unconsumed input after parsing Content-Encoding header: " <> show rest
      Fail -> Left "Failed to parse Content-Encoding header"
      Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderContentEncoding


  headerName _ = hContentEncoding


contentEncodingParser :: ParserT st String ContentEncoding
contentEncodingParser = ContentEncoding <$> contentCodingParser


renderContentEncoding :: ContentEncoding -> M.Builder
renderContentEncoding (ContentEncoding encoding) = renderContentCoding encoding
