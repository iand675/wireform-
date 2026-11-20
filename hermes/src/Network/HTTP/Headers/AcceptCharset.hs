{- |
@Accept-Charset@ — a request header by which a client indicated which
character encodings (charsets) it was willing to accept in the
response, each with an optional @;q=@ quality weight.

The header is **deprecated** by RFC 9110 (servers SHOULD ignore it;
clients SHOULD NOT send it, because UTF-8 is now the de-facto universal
encoding). It still appears on the wire for legacy services, so we ship
a parser \/ renderer for completeness.

Wire syntax mirrors "Network.HTTP.Headers.AcceptLanguage": a comma-separated list of charset
tokens, each optionally followed by @;q=value@. The token @*@ is allowed
and stands for \"any other charset not explicitly listed\".

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-12.5.2>

See also: "Network.HTTP.Headers.Accept", "Network.HTTP.Headers.AcceptEncoding", "Network.HTTP.Headers.AcceptLanguage", "Network.HTTP.Headers.ContentType", "Network.HTTP.Headers.Vary".
-}
module Network.HTTP.Headers.AcceptCharset (
  AcceptCharset (..),
  WeightedCharset (..),
  acceptCharsetParser,
  renderAcceptCharset,
) where

import qualified Data.Text.Short as ST
import Network.HTTP.ContentNegotiation (negotiationList, parseNegotiationHeaders, renderWeight, weightParser)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAcceptCharset)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


{- | A charset token (e.g. @utf-8@) plus its quality weight.  The
@*@ wildcard is represented by the literal short text @"*"@.
-}
data WeightedCharset = WeightedCharset
  { charsetTag :: !ST.ShortText
  , charsetWeight :: !Double
  }
  deriving stock (Eq, Show)


newtype AcceptCharset = AcceptCharset
  { acceptCharset :: [WeightedCharset]
  }
  deriving stock (Eq, Show)


instance KnownHeader AcceptCharset where
  type ParseFailure AcceptCharset = String
  type Cardinality AcceptCharset = 'ZeroOrOne
  type Direction AcceptCharset = 'Request


  parseFromHeaders _ = fmap AcceptCharset . parseNegotiationHeaders "Accept-Charset" listParser


  renderToHeaders _ = M.toStrictByteString . renderAcceptCharset


  headerName _ = hAcceptCharset


acceptCharsetParser :: ParserT st String AcceptCharset
acceptCharsetParser = AcceptCharset <$> listParser


listParser :: ParserT st String [WeightedCharset]
listParser = negotiationList weightedCharsetParser
  where
    weightedCharsetParser = do
      tag <- rfc9110Token
      w <- weightParser
      pure $ WeightedCharset tag w


renderWeightedCharset :: WeightedCharset -> M.Builder
renderWeightedCharset (WeightedCharset tag w) =
  shortText tag
    <> renderWeight w


renderAcceptCharset :: AcceptCharset -> M.Builder
renderAcceptCharset (AcceptCharset xs) =
  M.intersperse ", " (map renderWeightedCharset xs)
