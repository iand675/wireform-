{- |
The @Early-Data@ request header is set by an intermediary to indicate that the
request was conveyed in TLS early data (0-RTT) and forwarded before the TLS
handshake completed, so the origin can decide whether to process it or reply
@425 (Too Early)@. Its value is a Structured Fields integer; the only defined
value is @1@.

@
  Early-Data = sf-integer
@

Spec: <https://www.rfc-editor.org/rfc/rfc8470.html#section-5.1> (RFC 8470 §5.1)

See also: "Network.HTTP.Headers.Via", "Network.HTTP.Headers.Forwarded", "Network.HTTP.Headers.RetryAfter".
-}
module Network.HTTP.Headers.EarlyData (
  EarlyData (..),
  earlyDataParser,
  renderEarlyData,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hEarlyData)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | An @Early-Data@ header value (a Structured Fields integer; defined value @1@).
newtype EarlyData = EarlyData {earlyDataValue :: Int}
  deriving stock (Eq, Show)


instance KnownHeader EarlyData where
  type ParseFailure EarlyData = String
  type Cardinality EarlyData = 'ZeroOrOne
  type Direction EarlyData = 'Request


  parseFromHeaders _ headers = case runParser earlyDataParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Early-Data header: " <> show rest
    Fail -> Left "Failed to parse Early-Data header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderEarlyData


  headerName _ = hEarlyData


earlyDataParser :: ParserT st String EarlyData
earlyDataParser = EarlyData <$> rfc8941Integer


renderEarlyData :: EarlyData -> M.Builder
renderEarlyData (EarlyData n) = M.intDec n
