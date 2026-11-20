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


  parseFromHeaders _ = parseSingleFieldLine "Content-Length" contentLengthParser


  renderToHeaders _ = M.toStrictByteString . renderContentLength


  headerName _ = hContentLength


contentLengthParser :: ParserT st String ContentLength
contentLengthParser = ContentLength <$> boundedDecimalWord maxBound (err "Content-Length exceeds Word")


renderContentLength :: ContentLength -> M.Builder
renderContentLength (ContentLength len) = M.wordDec len
