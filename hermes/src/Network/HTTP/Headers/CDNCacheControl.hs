{- |
Module      : Network.HTTP.Headers.CDNCacheControl
Description : The @CDN-Cache-Control@ response header

The @CDN-Cache-Control@ header field is a targeted cache-control header,
allowing an origin to direct content delivery network (CDN) caches without
affecting other caches (such as browser caches) on the delivery path. It uses
exactly the same syntax and directive registry as 'Cache-Control'
(RFC 9111), so the directive bag is reused verbatim.

See <https://datatracker.ietf.org/doc/html/rfc9213> for the official
specification (RFC 9213: Targeted HTTP Cache Control).

See also: "Network.HTTP.Headers.CacheControl", "Network.HTTP.Headers.SurrogateControl", "Network.HTTP.Headers.CacheStatus", "Network.HTTP.Headers.CDNLoop".
-}
module Network.HTTP.Headers.CDNCacheControl (
  CDNCacheControl (..),
  cdnCacheControlParser,
  renderCDNCacheControl,
) where

import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup (sconcat)
import Network.HTTP.Headers
import Network.HTTP.Headers.CacheControl (
  CacheControlDirective (..),
  cacheControlHeaderParser,
  parseCacheControlHeader,
 )
import Network.HTTP.Headers.HeaderFieldName (hCDNCacheControl)
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (shortText)


{- | A @CDN-Cache-Control@ value: a non-empty list of cache directives,
sharing the 'CacheControlDirective' grammar of 'Cache-Control'.
-}
newtype CDNCacheControl = CDNCacheControl {cdnCacheControlDirectives :: NonEmpty CacheControlDirective}
  deriving stock (Eq, Show)


instance KnownHeader CDNCacheControl where
  type ParseFailure CDNCacheControl = String
  type Cardinality CDNCacheControl = 'ZeroOrMore
  type Direction CDNCacheControl = 'Response


  parseFromHeaders _ neHeaders =
    CDNCacheControl . sconcat <$> traverse parseCacheControlHeader neHeaders
  renderToHeaders _ = pure . M.toStrictByteString . renderCDNCacheControl
  headerName _ = hCDNCacheControl


cdnCacheControlParser :: ParserT st e CDNCacheControl
cdnCacheControlParser = CDNCacheControl <$> cacheControlHeaderParser


renderCDNCacheControl :: CDNCacheControl -> M.Builder
renderCDNCacheControl (CDNCacheControl ds) =
  M.intersperse ", " (fmap renderDirective ds)


renderDirective :: CacheControlDirective -> M.Builder
renderDirective = \case
  Public -> "public"
  Private Nothing -> "private"
  Private (Just v) -> "private=" <> shortText v
  NoCache Nothing -> "no-cache"
  NoCache (Just v) -> "no-cache=" <> shortText v
  NoStore -> "no-store"
  MaxAge v -> "max-age=" <> M.wordDec v
  SMaxAge v -> "s-maxage=" <> M.wordDec v
  MustRevalidate -> "must-revalidate"
  ProxyRevalidate -> "proxy-revalidate"
  NoTransform -> "no-transform"
  Immutable -> "immutable"
  StaleWhileRevalidate v -> "stale-while-revalidate=" <> M.wordDec v
  StaleIfError v -> "stale-if-error=" <> M.wordDec v
  Unknown k Nothing -> shortText k
  Unknown k (Just v) -> shortText k <> "=" <> shortText v
