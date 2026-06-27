{- |
RFC 9110 §8.6 @Content-Length@ — gives the size of the message body, in
octets, as a non-negative decimal integer. On a message with content it
states the length of the enclosed body; on a body-less message (e.g. a
@HEAD@ response) it anticipates the length the equivalent @GET@ would
return. It is a request and response header and acts as an alternative to
@Transfer-Encoding@ for determining message-body length; the two are
mutually exclusive.

== Grammar

@
Content-Length = 1*DIGIT
@

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-8.6>

See also: "Network.HTTP.Headers.TransferEncoding", "Network.HTTP.Headers.TE",
"Network.HTTP.Headers.ContentRange".
-}
module Network.HTTP.Headers.ContentLength (
  ContentLength (..),
  contentLengthParser,
  renderContentLength,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


newtype ContentLength = ContentLength {contentLength :: Word}
  deriving stock (Eq, Show)


instance KnownHeader ContentLength where
  type ParseFailure ContentLength = String
  type Cardinality ContentLength = 'ZeroOrOne
  type Direction ContentLength = 'RequestAndResponse


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser contentLengthParser header of
      OK cl "" -> Right cl
      OK _ rest -> Left $ "Unconsumed input after parsing Content-Length header: " <> show rest
      Fail -> Left "Failed to parse Content-Length header"
      Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderContentLength


  headerName _ = hContentLength


contentLengthParser :: ParserT st String ContentLength
contentLengthParser = ContentLength <$> anyAsciiDecimalWord


renderContentLength :: ContentLength -> M.Builder
renderContentLength (ContentLength len) = M.wordDec len
