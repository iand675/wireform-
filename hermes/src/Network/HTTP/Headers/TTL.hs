{- | TTL request header (RFC 8030 §5.2): the number of seconds a push message
is retained by the push service for later delivery to a user agent that is
currently offline, expressed as a single non-negative delta-seconds value. A
value of zero asks the push service to attempt immediate delivery only and not
store the message.

Spec: <https://www.rfc-editor.org/rfc/rfc8030#section-5.2>

See also: "Network.HTTP.Headers.Topic", "Network.HTTP.Headers.Urgency",
"Network.HTTP.Headers.Expires", "Network.HTTP.Headers.RetryAfter".
-}
module Network.HTTP.Headers.TTL (
  TTL (..),
  ttlParser,
  renderTTL,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hTTL)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | The push-message time-to-live, in seconds (RFC 8030 §5.2).
newtype TTL = TTL {ttlSeconds :: Word}
  deriving stock (Eq, Show)


instance KnownHeader TTL where
  type ParseFailure TTL = String
  type Cardinality TTL = 'ZeroOrOne
  type Direction TTL = 'Request


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser ttlParser header of
      OK ttl "" -> Right ttl
      OK _ rest -> Left $ "Unconsumed input after parsing TTL header: " <> show rest
      Fail -> Left "Failed to parse TTL header"
      Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderTTL


  headerName _ = hTTL


ttlParser :: ParserT st String TTL
ttlParser = TTL <$> anyAsciiDecimalWord


renderTTL :: TTL -> M.Builder
renderTTL (TTL seconds) = M.wordDec seconds
