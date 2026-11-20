{- |
@Age@ is a response header giving a cache's estimate, in seconds, of how long
ago the origin server generated (or last successfully validated) the response
now being served. Clients and intermediaries combine it with @Cache-Control@
and @Expires@ when
computing a stored response's remaining freshness lifetime.

Spec: <https://www.rfc-editor.org/rfc/rfc9111#section-5.1>

See also: "Network.HTTP.Headers.CacheControl", "Network.HTTP.Headers.Expires", "Network.HTTP.Headers.CacheStatus", "Network.HTTP.Headers.Date".
-}
module Network.HTTP.Headers.Age (
  Age (..),
  parseAge,
  renderAge,
) where

import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import Data.Word (Word32)
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAge)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Wireform.Parser.Driver (parseByteString)


newtype Age = Age {age :: Word32}
  deriving stock (Eq, Show)


instance KnownHeader Age where
  type ParseFailure Age = String
  type Cardinality Age = 'ZeroOrOne
  type Direction Age = 'Response


  -- Repeated field lines have the same first-member semantics as a comma list.
  parseFromHeaders _ headers =
    let wire = NE.head headers
        acceptShort = Right . Age . fromIntegral
        parseField = case parseByteString (ows *> parseAge <* ows <* (eof <|> skipSatisfyAscii (== ','))) wire of
          Right value -> Right value
          Left _ -> Left "Failed to parse Age header"
    in withShortDecimal wire acceptShort $
         case BS.elemIndex 44 (BS.take 10 wire) of
           Just comma -> withShortDecimal (BS.take comma wire) acceptShort parseField
           Nothing -> parseField


  renderToHeaders _ = M.toStrictByteString . renderAge


  headerName _ = hAge


parseAge :: ParserT st e Age
parseAge = Age . fromIntegral <$> saturatingDeltaSeconds (fromIntegral (maxBound :: Word32))


renderAge :: Age -> M.Builder
renderAge (Age a) = M.word32Dec a
