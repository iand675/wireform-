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
each visible ASCII node is preserved verbatim. Field lines contribute to the
same ordered chain.

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

import Data.Bits (complement, xor, (.&.), (.|.))
import qualified Data.CharSet as CS
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Data.Word (Word64)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hXForwardedFor)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (encodeCommaSeparatedText, renderHttpParameterList)


{- | A non-empty, comma-ordered chain of forwarding node identifiers, each
preserved as the raw token received (closest client first, by convention).
-}
newtype XForwardedFor = XForwardedFor {xForwardedForNodes :: NE.NonEmpty ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader XForwardedFor where
  type ParseFailure XForwardedFor = String
  type Cardinality XForwardedFor = 'ZeroOrOne
  type Direction XForwardedFor = 'Request


  parseFromHeaders _ (line NE.:| []) = parseFieldLine "X-Forwarded-For" xForwardedForParser line
  parseFromHeaders _ fields =
    XForwardedFor <$> (parseListFieldLines "X-Forwarded-For" (httpList node) fields >>= requireNonEmptyList "X-Forwarded-For")


  renderToHeaders _ (XForwardedFor nodes) = encodeCommaSeparatedText nodes


  headerName _ = hXForwardedFor


xForwardedForParser :: ParserT st String XForwardedFor
xForwardedForParser = do
  ows
  skipMany (comma *> ows)
  first <- node <* ows
  rest <- withOption comma (const (httpList node)) (pure [])
  pure (XForwardedFor (first NE.:| rest))
  where
    comma = skipSatisfyAscii (== ',')


node :: ParserT st String ST.ShortText
node = shortASCIIFromParser_ (scanAsciiSet visibleNodeWord nodeChars)
{-# INLINE node #-}


nodeChars :: AsciiSet
nodeChars = asciiSet (CS.fromList (['!' .. '+'] <> ['-' .. '~']))
{-# NOINLINE nodeChars #-}


-- Check eight visible ASCII bytes at once. A comma ends the node.
-- The high-bit bias keeps the inclusive range tests within each lane.
visibleNodeWord :: Word64 -> Bool
visibleNodeWord bytes =
  ((biased - 0x2121_2121_2121_2121) .&. (0xfefe_fefe_fefe_fefe - low) .&. complement bytes .&. high == high)
    && ((commas - 0x0101_0101_0101_0101) .&. complement commas .&. high == 0)
  where
    high = 0x8080_8080_8080_8080
    low = bytes .&. complement high
    biased = bytes .|. high
    commas = bytes `xor` 0x2c2c_2c2c_2c2c_2c2c
{-# INLINE visibleNodeWord #-}


renderXForwardedFor :: XForwardedFor -> M.Builder
renderXForwardedFor (XForwardedFor nodes) = renderHttpParameterList id (const Nothing) (NE.toList nodes)
