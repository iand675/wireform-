{- | Topic request header (RFC 8030 §5.4): an identifier a push service uses to
collapse (replace) previously stored, undelivered push messages that share the
same topic, so the user agent ultimately receives only the most recent one. Its
value is at most 32 characters drawn from the URL- and filename-safe Base64
alphabet (RFC 4648 §5): @ALPHA \/ DIGIT \/ "-" \/ "_"@. Parsing is lenient
about the 32-character upper bound; the value is preserved verbatim.

Spec: <https://www.rfc-editor.org/rfc/rfc8030#section-5.4>

See also: "Network.HTTP.Headers.TTL", "Network.HTTP.Headers.Urgency".
-}
module Network.HTTP.Headers.Topic (
  Topic (..),
  topicParser,
  renderTopic,
) where

import Data.CharSet (CharSet)
import qualified Data.CharSet as CharSet
import Data.CharSet.Posix.Ascii (alnum)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
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


  parseFromHeaders _ headers = do
    let header = NE.head headers
    case runParser topicParser header of
      OK topic "" -> Right topic
      OK _ rest -> Left $ "Unconsumed input after parsing Topic header: " <> show rest
      Fail -> Left "Failed to parse Topic header"
      Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderTopic


  headerName _ = hTopic


-- | The URL- and filename-safe Base64 alphabet (RFC 4648 §5).
topicCharSet :: CharSet
topicCharSet = alnum <> CharSet.fromList "-_"


topicParser :: ParserT st String Topic
topicParser = Topic <$> shortASCIIFromParser_ (some (satisfyAscii (`CharSet.member` topicCharSet)))


renderTopic :: Topic -> M.Builder
renderTopic (Topic v) = shortText v
