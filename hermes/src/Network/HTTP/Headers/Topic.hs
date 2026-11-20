{- | Topic request header (RFC 8030 §5.4): an identifier a push service uses to
collapse (replace) previously stored, undelivered push messages that share the
same topic, so the user agent ultimately receives only the most recent one. Its
value is at most 32 characters drawn from the URL- and filename-safe Base64
alphabet (RFC 4648 §5): @ALPHA \/ DIGIT \/ "-" \/ "_"@. The value is an
opaque, case-sensitive token with its original spelling preserved.

Spec: <https://www.rfc-editor.org/rfc/rfc8030#section-5.4>

See also: "Network.HTTP.Headers.TTL", "Network.HTTP.Headers.Urgency".
-}
module Network.HTTP.Headers.Topic (
  Topic (..),
  topicParser,
  renderTopic,
) where

import qualified Data.ByteString as BS
import qualified Data.Text.Short as ST
import qualified Data.Text.Short.Unsafe as STU
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hTopic)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


-- | A push-message collapse identifier (RFC 8030 §5.4).
newtype Topic = Topic {topicValue :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader Topic where
  type ParseFailure Topic = String
  type Cardinality Topic = 'ZeroOrOne
  type Direction Topic = 'Request


  parseFromHeaders _ = parseSingleFieldLine "Topic" topicParser


  renderToHeaders _ = ST.toByteString . topicValue


  headerName _ = hTopic


topicParser :: ParserT st String Topic
topicParser =
  withByteString base64UrlBytes $ \() wire ->
    if BS.length wire <= topicLengthLimit
      then pure (Topic (STU.fromByteStringUnsafe wire))
      else err "Topic exceeds 32 characters"


topicLengthLimit :: Int
topicLengthLimit = 32


renderTopic :: Topic -> M.Builder
renderTopic (Topic v) = shortText v
