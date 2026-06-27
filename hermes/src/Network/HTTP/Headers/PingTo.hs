{- |
The @Ping-To@ request header is part of hyperlink auditing: when a user
follows a link whose @ping@ attribute lists audit URLs, the browser sends a
@POST@ to each of them, and @Ping-To@ carries the URL the hyperlink points at
(the destination the user is navigating to). It is a de-facto header defined
by the WHATWG HTML standard rather than an IANA-registered field.

Spec (WHATWG HTML, hyperlink auditing): <https://html.spec.whatwg.org/multipage/links.html#hyperlink-auditing>

See also: "Network.HTTP.Headers.PingFrom", "Network.HTTP.Headers.Referer", "Network.HTTP.Headers.Origin".
-}
module Network.HTTP.Headers.PingTo where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers (HeaderCardinality (..), HeaderIsRequestOrResponse (..), KnownHeader (..))
import Network.HTTP.Headers.HeaderFieldName (hPingTo)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


newtype PingTo = PingTo {pingTo :: String}
  deriving stock (Eq, Show)


instance KnownHeader PingTo where
  type ParseFailure PingTo = String
  type Cardinality PingTo = 'ZeroOrOne
  type Direction PingTo = 'Request


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser pingToParser header of
      OK pt "" -> Right pt
      OK _ rest -> Left $ "Unconsumed input after parsing Ping-To header: " <> show rest
      Fail -> Left "Failed to parse Ping-To header"
      Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderPingTo


  headerName _ = hPingTo


pingToParser :: ParserT st String PingTo
pingToParser = PingTo <$> takeRestString


renderPingTo :: PingTo -> M.Builder
renderPingTo (PingTo str) = M.string8 str
