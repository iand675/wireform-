{- |
The @AMP-Cache-Transform@ HTTP header field, used in the AMP Signed Exchange
(SXG) negotiation between AMP caches and origin servers. Its value is an
RFC 8941 parameterised list of cache transform identifiers, e.g.

@
AMP-Cache-Transform: google;v="1..100"
@

Each member is a token naming a cache (such as @google@), optionally carrying a
@v@ parameter whose value constrains the acceptable AMP transform version (a
single integer or an @x..y@ range). This header is provisional and
AMP-specific; the version-range parameter grammar is not an IETF standard.

We therefore preserve the value verbatim in a newtype rather than fabricating a
structured decoding of the parameterised list, mirroring the treatment of other
opaque provisional fields.

Spec: <https://github.com/ampproject/amphtml/blob/main/docs/spec/amp-cache-transform.md>

See also: "Network.HTTP.Headers.CDNCacheControl", "Network.HTTP.Headers.CDNLoop", "Network.HTTP.Headers.CacheStatus", "Network.HTTP.Headers.Vary".
-}
module Network.HTTP.Headers.AMPCacheTransform (
  AMPCacheTransform (..),
  ampCacheTransformParser,
  renderAMPCacheTransform,
) where

import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Short as ST
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName (hAMPCacheTransform)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


{- | @AMP-Cache-Transform@ value: the parameterised list of cache transform
identifiers, preserved verbatim.
-}
newtype AMPCacheTransform = AMPCacheTransform {ampCacheTransformValue :: ST.ShortText}
  deriving stock (Eq, Show)


instance KnownHeader AMPCacheTransform where
  type ParseFailure AMPCacheTransform = String
  type Cardinality AMPCacheTransform = 'ZeroOrOne
  type Direction AMPCacheTransform = 'RequestAndResponse


  parseFromHeaders _ headers = case runParser ampCacheTransformParser (NE.head headers) of
    OK v "" -> Right v
    OK _ rest -> Left $ "Unconsumed input after parsing AMP-Cache-Transform header: " <> show rest
    Fail -> Left "Failed to parse AMP-Cache-Transform header"
    Err e -> Left e


  renderToHeaders _ = M.toStrictByteString . renderAMPCacheTransform


  headerName _ = hAMPCacheTransform


ampCacheTransformParser :: ParserT st String AMPCacheTransform
ampCacheTransformParser = AMPCacheTransform <$> takeRestShortText


renderAMPCacheTransform :: AMPCacheTransform -> M.Builder
renderAMPCacheTransform (AMPCacheTransform v) = shortText v
