{-# LANGUAGE TemplateHaskell #-}

{- |
The @Early-Data@ request header is set by an intermediary to indicate that the
request was conveyed in TLS early data (0-RTT) and forwarded before the TLS
handshake completed, so the origin can decide whether to process it or reply
@425 (Too Early)@. The wire value is the literal @1@. RFC 8470 requires
servers to interpret any present value, including malformed or repeated
field lines, as that marker.

@
  Early-Data = "1"
@

Spec: <https://www.rfc-editor.org/rfc/rfc8470.html#section-5.1> (RFC 8470 §5.1)

See also: "Network.HTTP.Headers.Via", "Network.HTTP.Headers.Forwarded", "Network.HTTP.Headers.RetryAfter".
-}
module Network.HTTP.Headers.EarlyData (
  EarlyData (..),
  earlyDataParser,
  renderEarlyData,
) where

import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hEarlyData)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


{- | The @Early-Data@ marker. The integer field is retained for source
compatibility. The typed codec decodes and emits the sole defined value, @1@.
-}
newtype EarlyData = EarlyData {earlyDataValue :: Int}
  deriving stock (Eq, Show)


instance KnownHeader EarlyData where
  type ParseFailure EarlyData = String
  type Cardinality EarlyData = 'ZeroOrOne
  type Direction EarlyData = 'Request


  parseFromHeaders _ _ = Right (EarlyData 1)


  renderToHeaders _ _ = "1"


  headerName _ = hEarlyData


-- | Recognize the authored wire value.
earlyDataParser :: ParserT st String EarlyData
earlyDataParser = EarlyData 1 <$ $(char '1')


renderEarlyData :: EarlyData -> M.Builder
renderEarlyData _ = "1"
