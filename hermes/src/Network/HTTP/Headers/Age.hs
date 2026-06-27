{- |
@Age@ is a response header giving a cache's estimate, in seconds, of how long
ago the origin server generated (or last successfully validated) the response
now being served. A value of @0@ implies a fresh response straight from the
origin; a positive value means the response came from a shared cache. Clients
and intermediaries combine it with @Cache-Control@ and @Expires@ when
computing a stored response's remaining freshness lifetime.

Spec: <https://www.rfc-editor.org/rfc/rfc9111#section-5.1>

See also: "Network.HTTP.Headers.CacheControl", "Network.HTTP.Headers.Expires", "Network.HTTP.Headers.CacheStatus", "Network.HTTP.Headers.Date".
-}
module Network.HTTP.Headers.Age (
  Age (..),
  parseAge,
  renderAge,
) where

import qualified Data.List.NonEmpty as NE
import Data.Word (Word32)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAge)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


newtype Age = Age {age :: Word32}
  deriving stock (Eq, Show)


instance KnownHeader Age where
  type ParseFailure Age = String
  type Cardinality Age = 'ZeroOrOne
  type Direction Age = 'Response


  parseFromHeaders _ headers = case runParser parseAge $ NE.head headers of
    OK a "" -> Right a
    OK _ rest -> Left $ "Unconsumed input after parsing Age header: " <> show rest
    Fail -> Left "Failed to parse Age header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderAge


  headerName _ = hAge


parseAge :: ParserT st e Age
parseAge = do
  {-
    We need to parse the delta-seconds value from the Age header field.
    Initially, Integer was considered to support the definition outlined
    in RFC 7234, but using integer allows for unbounded values, which is
    not what we want. Since 2^31 bits is the recommended minimum, Word64
    is sufficient for this purpose while retaining bounded memory use.

    From the delta-seconds definition in RFC 7234:
    https://datatracker.ietf.org/doc/html/rfc7234#section-1.2.1

    A recipient parsing a delta-seconds value and converting it to binary
    form ought to use an arithmetic type of at least 31 bits of
    non-negative integer range.  If a cache receives a delta-seconds
    value greater than the greatest integer it can represent, or if any
    of its subsequent calculations overflows, the cache MUST consider the
    value to be either 2147483648 (2^31) or the greatest positive integer
    it can conveniently represent.

    Note: The value 2147483648 is here for historical reasons,
    effectively represents infinity (over 68 years), and does not need
    to be stored in binary form; an implementation could produce it as
    a canned string if any overflow occurs, even if the calculations
    are performed with an arithmetic type incapable of directly
    representing that number.  What matters here is that an overflow
    be detected and not treated as a negative value in later
    calculations.
  -}
  w <- anyAsciiDecimalWord
  pure $
    Age $
      if w <= fromIntegral (maxBound :: Word32)
        then fromIntegral w
        else maxBound


renderAge :: Age -> M.Builder
renderAge (Age a) = M.word32Dec a
