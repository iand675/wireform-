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

import Data.List.NonEmpty (NonEmpty (..))
import Network.HTTP.Grammar.Rendering (encodeWordDecimal)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hMaxForwards)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


{- | A @Max-Forwards@ value: the remaining forwarding hop count.
The decoder accepts values through the largest nonnegative 'Int'.
-}
newtype MaxForwards = MaxForwards {maxForwards :: Int}
  deriving stock (Eq, Show)


instance KnownHeader MaxForwards where
  type ParseFailure MaxForwards = String
  type Cardinality MaxForwards = 'ZeroOrOne
  type Direction MaxForwards = 'Request


  parseFromHeaders _ fields = case fields of
    wire :| [] -> withShortDecimal wire (Right . MaxForwards) fallback
    _ -> fallback
    where
      fallback = parseSingleFieldLine "Max-Forwards" maxForwardsParser fields


  renderToHeaders _ = M.toStrictByteString . renderMaxForwards


  renderToHeadersChecked _ (MaxForwards hops)
    | hops >= 0 = Just (encodeWordDecimal (fromIntegral hops))
    | otherwise = Nothing
  {-# INLINE renderToHeadersChecked #-}


  headerName _ = hMaxForwards


maxForwardsParser :: ParserT st String MaxForwards
maxForwardsParser =
  MaxForwards . fromIntegral <$> boundedDecimalWord (fromIntegral (maxBound :: Int)) (err "Max-Forwards exceeds the supported range")


renderMaxForwards :: MaxForwards -> M.Builder
renderMaxForwards (MaxForwards n) = M.intDec n
