{- |
The @Ping-To@ request header is part of hyperlink auditing: when a user
follows a link whose @ping@ attribute lists audit URLs, the browser sends a
@POST@ to each of them, and @Ping-To@ carries the URL the hyperlink points at
(the destination the user is navigating to). It is a de-facto header defined
by the WHATWG HTML standard rather than an IANA-registered field.

Spec (WHATWG HTML, hyperlink auditing): <https://html.spec.whatwg.org/multipage/links.html#hyperlink-auditing>

See also: "Network.HTTP.Headers.PingFrom", "Network.HTTP.Headers.Referer", "Network.HTTP.Headers.Origin".
-}
module Network.HTTP.Headers.PingTo (
  PingTo (..),
  pingToParser,
  renderPingTo,
) where

import qualified Data.ByteString.Char8 as BSC
import Network.HTTP.Grammar.URIReference (browserURLBytes)
import Network.HTTP.Headers (HeaderCardinality (..), HeaderIsRequestOrResponse (..), KnownHeader (..))
import Network.HTTP.Headers.HeaderFieldName (hPingTo)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | The serialized URL, with its wire spelling preserved.
newtype PingTo = PingTo {pingTo :: String}
  deriving stock (Eq, Show)


instance KnownHeader PingTo where
  type ParseFailure PingTo = String
  type Cardinality PingTo = 'ZeroOrOne
  type Direction PingTo = 'Request


  parseFromHeaders _ = parseSingleFieldLine "Ping-To" pingToParser


  renderToHeaders _ = M.toStrictByteString . renderPingTo


  headerName _ = hPingTo


pingToParser :: ParserT st String PingTo
pingToParser = PingTo <$> withByteString browserURLBytes (\() bytes -> pure (BSC.unpack bytes))


renderPingTo :: PingTo -> M.Builder
renderPingTo (PingTo str) = M.string8 str
