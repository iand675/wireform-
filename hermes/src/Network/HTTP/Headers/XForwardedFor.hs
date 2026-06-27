{- |
@X-Forwarded-For@ — de-facto request header inserted by proxies, load
balancers, and caches to record the chain of client and intermediary node
addresses a request traversed.

This header is __not IANA-registered__: it predates and is superseded by the
standardized @Forwarded@ header
(<https://www.rfc-editor.org/rfc/rfc7239 RFC 7239>), but remains ubiquitous.
The de-facto syntax is a comma-separated list of node identifiers, ordered
closest-client-first by convention:

@
X-Forwarded-For = node *( OWS \",\" OWS node )
node            = IPv4address / \"[\" IPv6address \"]\" / token   ; optionally with a :port
@

Real-world deployments vary wildly — bare IPv6, bracketed IPv6, @:port@
suffixes, the literal @unknown@, and obfuscated @_hidden@ tokens all occur — so
each node is preserved verbatim as an opaque token rather than forced through a
single address grammar.

Spec (de-facto, not IANA-registered): MDN
<https://developer.mozilla.org/docs/Web/HTTP/Headers/X-Forwarded-For>.

See also: "Network.HTTP.Headers.Forwarded", "Network.HTTP.Headers.XRealIP",
"Network.HTTP.Headers.XForwardedHost", "Network.HTTP.Headers.XForwardedProto",
"Network.HTTP.Headers.Via".
-}
module Network.HTTP.Headers.XForwardedFor (
  XForwardedFor (..),
  xForwardedForParser,
  renderXForwardedFor,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXForwardedFor)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


{- | A non-empty, comma-ordered chain of forwarding node identifiers, each
preserved as the raw token received (closest client first, by convention).
-}
newtype XForwardedFor = XForwardedFor {xForwardedForNodes :: NE.NonEmpty ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader XForwardedFor where
  type ParseFailure XForwardedFor = String
  type Cardinality XForwardedFor = 'ZeroOrOne
  type Direction XForwardedFor = 'Request


  parseFromHeaders _ headers = case runParser xForwardedForParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing X-Forwarded-For header: " <> show rest
    Fail -> Left "Failed to parse X-Forwarded-For header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderXForwardedFor


  headerName _ = hXForwardedFor


xForwardedForParser :: ParserT st String XForwardedFor
xForwardedForParser = do
  first <- node
  rest <- many (ows *> skipSatisfyAscii (== ',') *> ows *> node)
  pure $ XForwardedFor (first NE.:| rest)
  where
    node = shortASCIIFromParser_ (skipSome (satisfyAscii isNodeChar))
    isNodeChar c = c /= ',' && c /= ' ' && c /= '\t'


renderXForwardedFor :: XForwardedFor -> M.Builder
renderXForwardedFor (XForwardedFor nodes) =
  M.intersperse ", " $ map shortText $ NE.toList nodes
