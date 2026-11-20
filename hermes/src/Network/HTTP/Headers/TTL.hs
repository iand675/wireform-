{- | TTL header (RFC 8030 §5.2): the number of seconds a push message
is retained by the push service for later delivery to a user agent that is
currently offline, expressed as a single non-negative delta-seconds value. A
value of zero asks the push service to attempt immediate delivery only and not
store the message. A push service can return TTL on its response to indicate
the actual retention time.

Values outside the range of 'Word' decode to 2^31 as specified by RFC 8030.

Spec: <https://www.rfc-editor.org/rfc/rfc8030#section-5.2>

See also: "Network.HTTP.Headers.Topic", "Network.HTTP.Headers.Urgency",
"Network.HTTP.Headers.Expires", "Network.HTTP.Headers.RetryAfter".
-}
module Network.HTTP.Headers.TTL (
  TTL (..),
  ttlParser,
  renderTTL,
) where

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
  type Direction TTL = 'RequestAndResponse


  parseFromHeaders _ = parseSingleFieldLine "TTL" ttlParser


  renderToHeaders _ = M.toStrictByteString . renderTTL


  headerName _ = hTTL


ttlParser :: ParserT st String TTL
ttlParser = TTL <$> boundedDecimalWord maxBound (2_147_483_648 <$ skipMany (skipSatisfyAscii isDigit))


renderTTL :: TTL -> M.Builder
renderTTL (TTL seconds) = M.wordDec seconds
