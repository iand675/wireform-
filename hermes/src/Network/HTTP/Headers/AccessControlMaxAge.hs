{- |
@Access-Control-Max-Age@ (WHATWG Fetch / W3C CORS) response header indicating
how many seconds (delta-seconds) the results of a preflight request — the allowed
methods and headers — may be cached by the browser. A value of @0@ disables
preflight caching.

== Grammar

@
Access-Control-Max-Age = delta-seconds
@

Spec: <https://fetch.spec.whatwg.org/#http-access-control-max-age>

See also: "Network.HTTP.Headers.AccessControlAllowMethods", "Network.HTTP.Headers.AccessControlAllowHeaders", "Network.HTTP.Headers.AccessControlRequestMethod", "Network.HTTP.Headers.AccessControlRequestHeaders".
-}
module Network.HTTP.Headers.AccessControlMaxAge (
  AccessControlMaxAge (..),
  accessControlMaxAgeParser,
  renderAccessControlMaxAge,
) where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty (..))
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAccessControlMaxAge)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util


-- | Non-negative age in seconds. Rendering clamps a negative input to zero.
newtype AccessControlMaxAge = AccessControlMaxAge {maxAgeSeconds :: Int}
  deriving stock (Eq, Show)


instance KnownHeader AccessControlMaxAge where
  type ParseFailure AccessControlMaxAge = String
  type Cardinality AccessControlMaxAge = 'ZeroOrOne
  type Direction AccessControlMaxAge = 'Response


  parseFromHeaders _ fields@(wire :| []) =
    withShortDecimal wire (Right . AccessControlMaxAge) (parseMaxAgeField fields)
  parseFromHeaders _ _ = Left "Multiple Access-Control-Max-Age field lines"


  renderToHeaders _ = M.toStrictByteString . renderAccessControlMaxAge


  headerName _ = hAccessControlMaxAge


accessControlMaxAgeParser :: ParserT st String AccessControlMaxAge
accessControlMaxAgeParser =
  AccessControlMaxAge . fromIntegral <$> saturatingDeltaSeconds (fromIntegral (maxBound :: Int))


parseMaxAgeField :: NonEmpty ByteString -> Either String AccessControlMaxAge
parseMaxAgeField = parseSingleFieldLine "Access-Control-Max-Age" accessControlMaxAgeParser


renderAccessControlMaxAge :: AccessControlMaxAge -> M.Builder
renderAccessControlMaxAge (AccessControlMaxAge n) = M.intDec (max 0 n)
