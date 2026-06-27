{- |
The @Ping-From@ request header is part of hyperlink auditing: when a user
follows a link whose @ping@ attribute lists audit URLs, the browser sends a
@POST@ to each of them, and @Ping-From@ carries the URL of the document that
contained the hyperlink. It is a de-facto header defined by the WHATWG HTML
standard rather than an IANA-registered field.

Spec (WHATWG HTML, hyperlink auditing): <https://html.spec.whatwg.org/multipage/links.html#hyperlink-auditing>

See also: "Network.HTTP.Headers.PingTo", "Network.HTTP.Headers.Referer", "Network.HTTP.Headers.Origin".
-}
module Network.HTTP.Headers.PingFrom where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hPingFrom)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


newtype PingFrom = PingFrom {pingFrom :: String}
  deriving stock (Eq, Show)


instance KnownHeader PingFrom where
  type ParseFailure PingFrom = String
  type Cardinality PingFrom = 'ZeroOrOne
  type Direction PingFrom = 'Request


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser pingFromParser header of
      OK pf "" -> Right pf
      OK _ rest -> Left $ "Unconsumed input after parsing Ping-From header: " <> show rest
      Fail -> Left "Failed to parse Ping-From header"
      Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderPingFrom


  headerName _ = hPingFrom


pingFromParser :: ParserT st String PingFrom
pingFromParser = PingFrom <$> takeRestString


renderPingFrom :: PingFrom -> M.Builder
renderPingFrom (PingFrom str) = M.string8 str
