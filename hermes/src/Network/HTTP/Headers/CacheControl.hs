{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- |
Module      : Network.HTTP.Headers.CacheControl
Description : Representation of HTTP Cache-Control directives

Currently targeting: HTTPWG RFC 9111

HTTP caching is a critical part of web performance, reducing latency and server load by reusing previously fetched resources.
The Cache-Control header plays a central role in this system, defining the caching policies directly within HTTP responses.

This module provides a data type to represent the various directives that can be specified in a Cache-Control header,
allowing developers to parse, manipulate, and generate these directives programmatically.

See <https://httpwg.org/specs/rfc9111.html> for the official specification of the Cache-Control header.

It is important to note that the Cache-Control MUST be respected by all caching mechanisms, including private caches and shared caches,
along the request/response chain. If a directive is not understood, it MUST be treated as if it had the value "no-cache".

Notes on Cache-Control Directives:

__Cache-Control and Expires__

In general @Cache-Control@ is considered more flexible and powerful. The Expires header specifies an absolute expiration date and time, whereas @Cache-Control@ can define caching behavior with greater granularity (e.g., max-age) and specificity (e.g., no-store, no-cache). When both headers are present,
Cache-Control takes precedence, providing more control over how caching mechanisms should interpret the freshness of a resource.

__Cache-Control and ETag/Last-Modified__

ETag and Last-Modified: These headers provide validators for cached responses.
The ETag (Entity Tag) header gives a unique identifier for the version of the resource,
while Last-Modified provides a timestamp of when the resource was last changed.
When a browser has a cached but potentially stale response,
it can use these values in conditional requests (If-None-Match for ETag, If-Modified-Since for Last-Modified)
to check if the resource has changed. If the server responds with 304 Not Modified,
the cached version can be safely reused, reducing bandwidth and load.

Interplay with Cache-Control: The no-cache directive does not prevent caching but requires that the cache validates stored responses with the origin server before reuse, typically using ETag or Last-Modified values. must-revalidate further enforces that once a resource is stale, a validation must occur before its use, ensuring clients always receive up-to-date or validated content, even at the cost of additional round trips for revalidation.

__@Cache-Control@ and @Vary@__
Vary Header: The Vary header informs caches that a resource's response may vary based on the value of specified request headers.
For example, a response varying on the Accept-Encoding header would have different versions stored for requests that accept gzip
compression versus those that do not.

Interaction with Cache-Control: The presence of a Vary header can complicate caching decisions. It essentially creates a multi-dimensional
cache key based on the varying header fields and the request URL. Cache-Control directives still apply, but caches must also consider
the specific request headers listed in Vary to determine the correct version of the resource to deliver. This ensures content is accurately
served according to client capabilities or preferences but can increase the complexity of cache management.

__General Interplay__
The combined use of these headers provides a robust framework for web caching, allowing developers to fine-tune cache behavior to balance between load reduction and content freshness. Here's a typical flow illustrating their interplay:

Initial Request: A client requests a resource for the first time. The server responds with the resource, Cache-Control, ETag/Last-Modified, and optionally Vary.
Subsequent Requests: The client stores the resource in its cache according to Cache-Control policies.

Validation: Upon a cache hit for a subsequent request, if the resource is stale (max-age expired) or must-revalidate is specified, the client sends a conditional
request with If-None-Match (using ETag) or If-Modified-Since (using Last-Modified).

Server Response: The server either confirms the resource hasn't changed (with a 304 Not Modified response, allowing the cache to reuse the response) or sends a new version of the resource.
This system, while complex, enables efficient, flexible, and scalable web content delivery, minimizing unnecessary data transfers while ensuring users receive up-to-date content.

See also: "Network.HTTP.Headers.Expires", "Network.HTTP.Headers.Age", "Network.HTTP.Headers.Pragma", "Network.HTTP.Headers.CDNCacheControl", "Network.HTTP.Headers.Vary", "Network.HTTP.Headers.ETag".
-}
module Network.HTTP.Headers.CacheControl (
  CacheControl (..),
  CacheControlDirective (..),

  -- * Cache control header parsing and validation
  UsableDirectives (..),
  usableRequestDirectives,
  usableResponseDirectives,
  parseCacheControlHeader,
  cacheControlHeaderParser,
  directiveParser,
  validateCacheDirectives,

  -- * Cache control header value rendering
  directivesBuilder,
  directiveBuilder,
) where

import Control.Monad
import Control.Monad.Combinators (option)
import Control.Monad.Combinators.NonEmpty (sepBy1)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Foldable1
import Data.Hashable
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (sconcat)
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as TS
import Data.Validation (Validation (..))
import GHC.Generics
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName
import Network.HTTP.Headers.Parsing.Util
import Prelude


{- | 'validationNel' was removed from @Data.Validation@ when the library
was refactored for the 1.2 line. We reproduce it locally here so the
conflict-checking code below keeps reading the same way.
-}
validationNel :: Either e a -> Validation (NonEmpty e) a
validationNel (Left e) = Failure (e :| [])
validationNel (Right a) = Success a


newtype CacheControl = CacheControl {cacheControlDirectives :: NonEmpty CacheControlDirective}
  deriving stock (Eq, Show)


instance KnownHeader CacheControl where
  type ParseFailure CacheControl = String
  type Cardinality CacheControl = 'ZeroOrMore
  type Direction CacheControl = 'RequestAndResponse


  parseFromHeaders _ neHeaders = CacheControl . sconcat <$> traverse parseCacheControlHeader neHeaders
  renderToHeaders _ = pure . BL.toStrict . BB.toLazyByteString . directivesBuilder . cacheControlDirectives
  headerName _ = hCacheControl


{- | Represents the directives used in HTTP `Cache-Control` headers.

See <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Cache-Control>
for more information.

Note that this type is not exhaustive and may not include all possible directives.
-}
data CacheControlDirective
  = -- | Indicates the response can be cached by any cache.
    Public
  | -- | The response is for a single user and should not be stored by shared caches.
    --     Optional field names can specify parts of the response intended for a single user.
    Private (Maybe ShortText)
  | -- | Forces caches to validate the response with the origin server before reuse.
    --     Optional field names can specify parts of the response to always validate.
    NoCache (Maybe ShortText)
  | -- | Directs caches not to store the response or request.
    NoStore
  | -- | Specifies the maximum age in seconds a resource is considered fresh.
    MaxAge Word
  | -- | Similar to 'MaxAge', but specifically for shared caches.
    SMaxAge Word
  | -- | Indicates that once stale, the cached response must be validated before use.
    MustRevalidate
  | -- | Similar to 'MustRevalidate', but applies only to shared caches.
    ProxyRevalidate
  | -- | Prohibits transformations of the response content.
    NoTransform
  | -- | Suggests the response will not change over time.
    Immutable
  | -- | Allows serving stale content while asynchronously checking for a fresh version.
    StaleWhileRevalidate Word
  | -- | Permits serving stale content if an error occurs when revalidating.
    StaleIfError Word
  | -- | Represents directives that are not recognized, optionally with a value.
    Unknown ShortText (Maybe ShortText)
  deriving stock (Show, Eq, Ord, Generic)


instance Hashable CacheControlDirective


data UsableDirectives = UsableDirectives
  { usableDirectives :: [CacheControlDirective]
  , unusableDirectives :: [CacheControlDirective]
  , unknownDirectives :: [(ShortText, Maybe ShortText)]
  }
  deriving stock (Show)


instance Semigroup UsableDirectives where
  UsableDirectives v1 i1 u1 <> UsableDirectives v2 i2 u2 = UsableDirectives (v1 <> v2) (i1 <> i2) (u1 <> u2)


instance Monoid UsableDirectives where
  mempty = UsableDirectives [] [] []


data DirectivePresence = DirectivePresence
  { hasNoCache :: Bool
  , hasNoStore :: Bool
  , hasPrivate :: Bool
  , hasPublic :: Bool
  , hasMaxAge :: Bool
  , hasSMaxAge :: Bool
  , hasNoTransform :: Bool
  , hasMustRevalidate :: Bool
  , hasProxyRevalidate :: Bool
  , hasImmutable :: Bool
  , hasStaleWhileRevalidate :: Bool
  , hasStaleIfError :: Bool
  }
  deriving (Show)


-- Initialize with all fields set to False.
initialPresence :: DirectivePresence
initialPresence = DirectivePresence False False False False False False False False False False False False


-- Update the presence tracker based on the given directive.
updatePresence :: CacheControlDirective -> DirectivePresence -> DirectivePresence
updatePresence directive presence = case directive of
  NoCache _ -> presence {hasNoCache = True}
  NoStore -> presence {hasNoStore = True}
  Private _ -> presence {hasPrivate = True}
  Public -> presence {hasPublic = True}
  MaxAge _ -> presence {hasMaxAge = True}
  SMaxAge _ -> presence {hasSMaxAge = True}
  MustRevalidate -> presence {hasMustRevalidate = True}
  ProxyRevalidate -> presence {hasProxyRevalidate = True}
  NoTransform -> presence {hasNoTransform = True}
  Immutable -> presence {hasImmutable = True}
  StaleWhileRevalidate _ -> presence {hasStaleWhileRevalidate = True}
  StaleIfError _ -> presence {hasStaleIfError = True}
  Unknown _ _ -> presence -- Unknown directives are ignored in conflict checks


-- Function to check for well-known directive conflicts based on the tracked presence.
checkConflicts :: DirectivePresence -> Validation (NonEmpty String) DirectivePresence
checkConflicts p@DirectivePresence {..} =
  p
    <$ foldMap1
      validationNel
      ( when
          (hasPrivate && hasPublic)
          (Left "private conflicts with public.")
          :| [ when
                ( hasNoStore
                    && or
                      [ hasPrivate
                      , hasPublic
                      , hasMaxAge
                      , hasSMaxAge
                      , hasMustRevalidate
                      , hasProxyRevalidate
                      , hasStaleWhileRevalidate
                      , hasStaleIfError
                      ]
                )
                (Left "no-store conflicts with directives that imply storage.")
             , when
                (hasPrivate && hasSMaxAge)
                (Left "private conflicts with s-maxage since proxies should not be storing content.")
             , when
                (hasImmutable && (hasStaleWhileRevalidate || hasStaleIfError))
                (Left "immutable conflicts with directives that imply stale content.")
             ]
      )


{- | Validate the list of CacheControlDirectives. This function returns a Left value with an error message if
there are conflicts between directives. The presence of unknown directives is ignored, although you MUST
handle them in a manner appropriate to your application logic.
-}
validateCacheDirectives :: [CacheControlDirective] -> Validation (NonEmpty String) DirectivePresence
validateCacheDirectives = checkConflicts . foldr updatePresence initialPresence


addValidDirective :: CacheControlDirective -> UsableDirectives -> UsableDirectives
addValidDirective dir (UsableDirectives valids invalids unknowns) = UsableDirectives (dir : valids) invalids unknowns


addInvalidDirective :: CacheControlDirective -> UsableDirectives -> UsableDirectives
addInvalidDirective dir (UsableDirectives valids invalids unknowns) = UsableDirectives valids (dir : invalids) unknowns


addUnknownDirective :: ShortText -> Maybe ShortText -> UsableDirectives -> UsableDirectives
addUnknownDirective k mv (UsableDirectives valids invalids unknowns) = UsableDirectives valids invalids ((k, mv) : unknowns)


usableRequestDirectives :: Foldable t => t CacheControlDirective -> UsableDirectives
usableRequestDirectives = foldr split mempty
  where
    split dir ds =
      let valid = addValidDirective dir ds
      in case dir of
          Public -> valid
          NoCache _ -> valid
          NoStore -> valid
          MaxAge _ -> valid
          NoTransform -> valid
          Unknown k mv -> addUnknownDirective k mv ds
          _ -> addInvalidDirective dir ds


usableResponseDirectives :: Foldable t => t CacheControlDirective -> UsableDirectives
usableResponseDirectives = foldr split mempty
  where
    split dir ds =
      let valid = addValidDirective dir ds
      in case dir of
          Public -> valid
          Private _ -> valid
          NoCache _ -> valid
          NoStore -> valid
          MaxAge _ -> valid
          SMaxAge _ -> valid
          MustRevalidate -> valid
          ProxyRevalidate -> valid
          NoTransform -> valid
          Immutable -> valid
          StaleWhileRevalidate _ -> valid
          StaleIfError _ -> valid
          Unknown k mv -> addUnknownDirective k mv ds


numericDirective :: (Word -> CacheControlDirective) -> ParserT st e CacheControlDirective
numericDirective f = do
  $(char '=')
  f <$> anyAsciiDecimalWord


optStringDirective :: (Maybe ShortText -> CacheControlDirective) -> ParserT st e CacheControlDirective
optStringDirective f = option (f Nothing) (f . Just <$> ($(char '=') *> (rfc9110Token <|> quotedString)))


directiveParser :: ParserT st e CacheControlDirective
directiveParser =
  $( switch
      [|
        case _ of
          "public" -> pure Public
          "private" -> optStringDirective Private
          "no-cache" -> optStringDirective NoCache
          "no-store" -> pure NoStore
          "max-age" -> numericDirective MaxAge
          "s-maxage" -> numericDirective SMaxAge
          "must-revalidate" -> pure MustRevalidate
          "proxy-revalidate" -> pure ProxyRevalidate
          "no-transform" -> pure NoTransform
          "immutable" -> pure Immutable
          "stale-while-revalidate" -> numericDirective StaleWhileRevalidate
          "stale-if-error" -> numericDirective StaleIfError
          _ -> do
            k <- rfc9110Token
            option (Unknown k Nothing) (Unknown k . Just <$> ($(char '=') *> (rfc9110Token <|> quotedString)))
        |]
   )


cacheControlHeaderParser :: ParserT st e (NonEmpty CacheControlDirective)
cacheControlHeaderParser = directiveParser `sepBy1` (ows *> $(char ',') *> ows)


parseCacheControlHeader :: ByteString -> Either String (NonEmpty CacheControlDirective)
parseCacheControlHeader bs = case runParser cacheControlHeaderParser bs of
  (OK dirs "") -> Right dirs
  (OK _ rest) -> Left $ "Unconsumed input after parsing Cache-Control header: " <> show rest
  Fail -> Left "Failed to parse Cache-Control header"
  (Err e) -> Left e


directivesBuilder :: NonEmpty CacheControlDirective -> BB.Builder
directivesBuilder = sconcat . NE.intersperse "," . fmap directiveBuilder


directiveBuilder :: CacheControlDirective -> BB.Builder
directiveBuilder = \case
  Public -> "public"
  Private Nothing -> "private"
  Private (Just v) -> "private=" <> BB.shortByteString (TS.toShortByteString v)
  NoCache Nothing -> "no-cache"
  NoCache (Just v) -> "no-cache=" <> BB.shortByteString (TS.toShortByteString v)
  NoStore -> "no-store"
  MaxAge v -> "max-age=" <> BB.wordDec v
  SMaxAge v -> "s-maxage=" <> BB.wordDec v
  MustRevalidate -> "must-revalidate"
  ProxyRevalidate -> "proxy-revalidate"
  NoTransform -> "no-transform"
  Immutable -> "immutable"
  StaleWhileRevalidate v -> "stale-while-revalidate=" <> BB.wordDec v
  StaleIfError v -> "stale-if-error=" <> BB.wordDec v
  Unknown k Nothing -> BB.shortByteString $ TS.toShortByteString k
  Unknown k (Just v) -> BB.shortByteString (TS.toShortByteString k) <> "=" <> BB.shortByteString (TS.toShortByteString v)
