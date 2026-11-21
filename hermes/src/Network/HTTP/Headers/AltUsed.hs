{- |
RFC 7838 §5 @Alt-Used@ — a request header sent by a client to
identify the alternative service it is actually using to reach the
origin.  It lets the alternative distinguish requests routed to it
from those made directly to the origin (and detect loops).

== Grammar (RFC 7838 §5)

@
Alt-Used = uri-host [ \":\" port ]
@

The value is a single @host@ with an optional @port@.  Bracketed
IPv6 literals (e.g. @[2001:db8::1]:443@) are preserved verbatim.

Spec: <https://www.rfc-editor.org/rfc/rfc7838#section-5>

See also: "Network.HTTP.Headers.AltSvc", "Network.HTTP.Headers.Host", "Network.HTTP.Headers.Via".
-}
module Network.HTTP.Headers.AltUsed (
  AltUsed (..),
  altUsedParser,
  renderAltUsed,
) where

import Data.Text.Short (ShortText)
import Data.Word (Word16)
import Network.HTTP.Grammar.Authority (encodeHostAndPortChecked, hostAndPort, renderHostAndPort)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAltUsed)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- ---------------------------------------------------------------------------
-- Data
-- ---------------------------------------------------------------------------

-- | @uri-host [ \":\" port ]@.
data AltUsed = AltUsed
  { altUsedHost :: !ShortText
  -- ^ The host (bracketed for IPv6 literals).
  , altUsedPort :: !(Maybe Word16)
  -- ^ The port, when present.
  }
  deriving stock (Eq, Show)


instance KnownHeader AltUsed where
  type ParseFailure AltUsed = String
  type Cardinality AltUsed = 'ZeroOrOne
  type Direction AltUsed = 'Request


  parseFromHeaders _ = parseSingleFieldLine "Alt-Used" altUsedParser


  renderToHeaders _ = M.toStrictByteString . renderAltUsed


  renderToHeadersChecked _ (AltUsed host port) = encodeHostAndPortChecked host port


  headerName _ = hAltUsed


-- ---------------------------------------------------------------------------
-- Parser
-- ---------------------------------------------------------------------------

altUsedParser :: ParserT st String AltUsed
altUsedParser = ows *> hostAndPort AltUsed


-- ---------------------------------------------------------------------------
-- Renderer
-- ---------------------------------------------------------------------------

renderAltUsed :: AltUsed -> M.Builder
renderAltUsed (AltUsed host port) = renderHostAndPort host port
