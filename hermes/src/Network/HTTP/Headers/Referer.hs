{- |
The @Referer@ request header gives the URI of the page from which the request
was initiated (the misspelling of \"referrer\" is baked into the protocol),
letting servers see where traffic came from. Browsers may trim or omit it
under a referrer policy for privacy. Its value is a URI reference without a fragment
or userinfo. The codec preserves its spelling.

@
Referer = absolute-URI / partial-URI
@

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-10.1.3>

See also: "Network.HTTP.Headers.RefererRoot", "Network.HTTP.Headers.Origin", "Network.HTTP.Headers.From", "Network.HTTP.Headers.PingFrom".
-}
module Network.HTTP.Headers.Referer (
  Referer (..),
  refererParser,
  renderReferer,
) where

import qualified Data.Text.Short as ST
import Network.HTTP.Grammar.URIReference (checkedURIField, outboundReferrerReferenceBytes, referrerReference)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hReferer)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | Referer header value containing the referring URI
newtype Referer = Referer {refererUri :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader Referer where
  type ParseFailure Referer = String
  type Cardinality Referer = 'ZeroOrOne
  type Direction Referer = 'Request


  parseFromHeaders _ = parseSingleFieldLine "Referer" refererParser


  renderToHeaders _ = M.toStrictByteString . renderReferer


  renderToHeadersChecked _ = checkedURIField outboundReferrerReferenceBytes . ST.toByteString . refererUri


  headerName _ = hReferer


refererParser :: ParserT st String Referer
refererParser = Referer <$> referrerReference


renderReferer :: Referer -> M.Builder
renderReferer (Referer uri) = shortText uri
