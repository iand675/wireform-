{- |
RFC 9110 §7.6.2 @Max-Forwards@ — limits the number of times a @TRACE@
or @OPTIONS@ request may be forwarded by intermediaries; each forwarding
recipient decrements the value.

== Grammar

@
Max-Forwards = 1*DIGIT
@

Spec: <https://www.rfc-editor.org/rfc/rfc9110#section-7.6.2>

See also: "Network.HTTP.Headers.Via", "Network.HTTP.Headers.Forwarded",
"Network.HTTP.Headers.Connection".
-}
module Network.HTTP.Headers.MaxForwards (
  MaxForwards (..),
  maxForwardsParser,
  renderMaxForwards,
) where

import qualified Data.List.NonEmpty as NE
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hMaxForwards)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | A @Max-Forwards@ value: the remaining forwarding hop count.
newtype MaxForwards = MaxForwards {maxForwards :: Int}
  deriving stock (Eq, Show)


instance KnownHeader MaxForwards where
  type ParseFailure MaxForwards = String
  type Cardinality MaxForwards = 'ZeroOrOne
  type Direction MaxForwards = 'Request


  parseFromHeaders _ headers = case runParser maxForwardsParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing Max-Forwards header: " <> show rest
    Fail -> Left "Failed to parse Max-Forwards header"
    Err e -> Left e
  renderToHeaders _ = M.toStrictByteString . renderMaxForwards
  headerName _ = hMaxForwards


maxForwardsParser :: ParserT st String MaxForwards
maxForwardsParser = MaxForwards <$> anyAsciiDecimalInt


renderMaxForwards :: MaxForwards -> M.Builder
renderMaxForwards (MaxForwards n) = M.intDec n
