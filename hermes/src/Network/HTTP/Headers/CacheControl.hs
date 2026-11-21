{-# LANGUAGE BangPatterns #-}
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

Caches ignore unrecognized directives. Forwarding proxies preserve them
because other recipients along the chain may implement the extension.

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
  cacheControlDirectiveList,
  cacheDirectiveFieldNames,
  privateFor,
  noCacheFor,
  parseCacheControlDirectives,
  parseCacheControlHeader,
  cacheControlHeaderParser,
  directiveParser,
  validateCacheDirectives,

  -- * Cache control header value rendering
  directivesBuilder,
  directiveBuilder,
  renderCacheDirective,
) where

import Control.Monad
import Control.Monad.Combinators (between)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Prim as Prim
import qualified Data.ByteString.Builder.Prim.Internal as Prim
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Short.Internal as SBS
import Data.Char (isAsciiUpper, toLower)
import qualified Data.CharSet as CharSet
import Data.Either (isLeft, isRight)
import Data.Foldable1
import Data.Hashable
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as TS
import qualified Data.Text.Short.Unsafe as TSU
import Data.Validation (Validation (..))
import Foreign.Ptr (minusPtr, plusPtr)
import GHC.Generics
import Network.HTTP.Headers
import Network.HTTP.Headers.HeaderFieldName
import qualified Network.HTTP.Headers.Mason as M
import Network.HTTP.Headers.Parsing.Util
import Network.HTTP.Headers.Rendering.Util (foldHttpQuotedShortBytes, renderHttpQuotedString, shortText)
import Wireform.Encode.Direct (dWord8)
import Wireform.Parser.Driver (parseByteString)
import Prelude


{- | 'validationNel' was removed from @Data.Validation@ when the library
was refactored for the 1.2 line. We reproduce it locally here so the
conflict-checking code below keeps reading the same way.
-}
validationNel :: Either e a -> Validation (NonEmpty e) a
validationNel (Left e) = Failure (e :| [])
validationNel (Right a) = Success a


{- | A present Cache-Control field. Use 'cacheControlDirectiveList' to
read both constructors. The record selector applies to populated fields.
-}
data CacheControl
  = CacheControl {cacheControlDirectives :: NonEmpty CacheControlDirective}
  | CacheControlEmpty
  deriving stock (Eq, Show)


instance KnownHeader CacheControl where
  type ParseFailure CacheControl = String
  type Cardinality CacheControl = 'ZeroOrMore
  type Direction CacheControl = 'RequestAndResponse


  parseFromHeaders _ fields =
    fmap (maybe CacheControlEmpty CacheControl . NE.nonEmpty) $ case fields of
      value :| [] -> parseCacheControlDirectives value
      _ -> parseListFieldLines "Cache-Control" (httpList directiveParser) fields
  renderToHeaders _ CacheControlEmpty = [""]
  renderToHeaders _ (CacheControl dirs) = [M.toStrictByteString (renderCacheDirectives dirs)]
  renderToHeadersChecked _ CacheControlEmpty = Just [""]
  renderToHeadersChecked _ (CacheControl dirs) = pure <$> encodeCacheDirectivesChecked dirs
  headerName _ = hCacheControl


{- | Represents the directives used in HTTP `Cache-Control` headers.

See <https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Cache-Control>
for more information.

Note that this type is not exhaustive and may not include all possible directives.
-}
data CacheControlDirective
  = -- | Indicates the response can be cached by any cache.
    Public
  | {- | The response is for a single user and should not be stored by shared caches.
    Optional field names can specify parts of the response intended for a single user.
    -}
    Private (Maybe ShortText)
  | {- | Forces caches to validate the response with the origin server before reuse.
    Optional field names can specify parts of the response to always validate.
    -}
    NoCache (Maybe ShortText)
  | -- | Directs caches not to store the response or request.
    NoStore
  | -- | Specifies the maximum age in seconds a resource is considered fresh.
    MaxAge Word
  | -- | Maximum acceptable staleness, or any staleness when no limit is supplied.
    MaxStale (Maybe Word)
  | -- | Minimum remaining freshness requested by the client, in seconds.
    MinFresh Word
  | -- | Similar to 'MaxAge', but specifically for shared caches.
    SMaxAge Word
  | -- | Indicates that once stale, the cached response must be validated before use.
    MustRevalidate
  | -- | Requires a cache to understand the response status code's caching rules.
    MustUnderstand
  | -- | Similar to 'MustRevalidate', but applies only to shared caches.
    ProxyRevalidate
  | -- | Prohibits transformations of the response content.
    NoTransform
  | -- | Requests an already stored response.
    OnlyIfCached
  | -- | Suggests the response will not change over time.
    Immutable
  | -- | Allows serving stale content while asynchronously checking for a fresh version.
    StaleWhileRevalidate Word
  | -- | Permits serving stale content if an error occurs when revalidating.
    StaleIfError Word
  | -- | An unrecognized directive. Quoted value octets use a one-to-one Latin-1 mapping.
    Unknown ShortText (Maybe ShortText)
  deriving stock (Show, Eq, Ord, Generic)


-- | All directives of a present field, in wire order.
cacheControlDirectiveList :: CacheControl -> [CacheControlDirective]
cacheControlDirectiveList CacheControlEmpty = []
cacheControlDirectiveList (CacheControl dirs) = NE.toList dirs


-- | The case-insensitive field names of a qualified directive.
cacheDirectiveFieldNames :: CacheControlDirective -> Maybe (NonEmpty HeaderFieldName)
cacheDirectiveFieldNames dir = case dir of
  Private arg -> arg >>= qualifiedFieldNames . TS.toByteString
  NoCache arg -> arg >>= qualifiedFieldNames . TS.toByteString
  _ -> Nothing


-- | Construct a qualified private directive from field names.
privateFor :: NonEmpty HeaderFieldName -> CacheControlDirective
privateFor = Private . Just . fieldNameListText


-- | Construct a qualified no-cache directive from field names.
noCacheFor :: NonEmpty HeaderFieldName -> CacheControlDirective
noCacheFor = NoCache . Just . fieldNameListText


fieldNameListText :: NonEmpty HeaderFieldName -> ShortText
fieldNameListText =
  TSU.fromByteStringUnsafe
    . M.toStrictByteString
    . M.intersperse ", "
    . fmap (M.shortByteString . headerNameToShortByteString)


qualifiedFieldNames :: ByteString -> Maybe (NonEmpty HeaderFieldName)
qualifiedFieldNames arg = do
  names <- either (const Nothing) Just (parseByteString (httpList rfc9110TokenBS <* eof) arg)
  fmap (unsafeUnknownHeaderFromBytestring . foldName) <$> NE.nonEmpty names


validCacheDirective :: CacheControlDirective -> Bool
validCacheDirective = \case
  Private fields -> all validFieldList fields
  NoCache fields -> all validFieldList fields
  Unknown key value -> isHttpTokenText key && isExtensionName key && all isHttpQuotedText value
  _ -> True
  where
    validFieldList text =
      isHttpTokenText text || isRight (parseByteString fieldList (TS.toByteString text))
    fieldList =
      ows *> skipMany separator *> tokenBytes <* ows
        <* skipMany (separator *> optional_ (tokenBytes <* ows)) <* eof
    separator = $(char ',') *> ows
    isExtensionName key =
      isLeft (parseByteString (withKnownCacheDirective (const eof)) (TS.toByteString key))


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
  Private Nothing -> presence {hasPrivate = True}
  Private (Just _) -> presence
  Public -> presence {hasPublic = True}
  MaxAge _ -> presence {hasMaxAge = True}
  MaxStale _ -> presence
  MinFresh _ -> presence
  SMaxAge _ -> presence {hasSMaxAge = True}
  MustRevalidate -> presence {hasMustRevalidate = True}
  MustUnderstand -> presence
  ProxyRevalidate -> presence {hasProxyRevalidate = True}
  NoTransform -> presence {hasNoTransform = True}
  OnlyIfCached -> presence
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
           NoCache Nothing -> valid
           NoStore -> valid
           MaxAge _ -> valid
           MaxStale _ -> valid
           MinFresh _ -> valid
           NoTransform -> valid
           OnlyIfCached -> valid
           StaleIfError _ -> valid
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
           MustUnderstand -> valid
           ProxyRevalidate -> valid
           NoTransform -> valid
           Immutable -> valid
           StaleWhileRevalidate _ -> valid
           StaleIfError _ -> valid
           Unknown k mv -> addUnknownDirective k mv ds
           _ -> addInvalidDirective dir ds


deltaSeconds :: ParserT st e Word
deltaSeconds = saturatingDeltaSeconds maxBound
{-# INLINE deltaSeconds #-}


directiveDelay :: ParserT st e Word
directiveDelay = deltaSeconds <|> between $(char '"') $(char '"') deltaSeconds
{-# INLINE directiveDelay #-}


qualifiedDirective :: (Maybe ShortText -> CacheControlDirective) -> ParserT st e CacheControlDirective
qualifiedDirective make = withOption $(char '=') (const argument) (pure (make Nothing))
  where
    argument =
      (make . Just <$> rfc9110Token) <|> do
        value <- httpQuotedBytes
        case qualifiedFieldNames value of
          Nothing -> failed
          Just _ -> pure (make (Just (TSU.fromByteStringUnsafe value)))


foldName :: ByteString -> ByteString
foldName key
  | BC.any isAsciiUpper key = BC.map toLower key
  | otherwise = key
{-# INLINE foldName #-}


directiveParser :: ParserT st e CacheControlDirective
directiveParser = embedError (withKnownCacheDirective whole <|> unknownDirective) (const failed)
  where
    -- An invalid argument for a known name must not become an extension.
    whole action =
      notFollowedBy (skipSatisfyAscii (\c -> CharSet.member c tokenCharSet)) *> (action <|> err ())
    unknownDirective = do
      name <- rfc9110Token
      withOption
        $(char '=')
        (const (Unknown name . Just <$> (rfc9110Token <|> (TS.fromText <$> httpQuotedString))))
        (pure (Unknown name Nothing))


-- Both the reader and the extension-name check use this exact dispatch.
withKnownCacheDirective
  :: (ParserT st e CacheControlDirective -> ParserT st e a)
  -> ParserT st e a
withKnownCacheDirective whole =
  $( switchAsciiCI
       [|
         case _ of
           "max-age" -> whole (numeric MaxAge)
           "no-cache" -> whole (qualifiedDirective NoCache)
           "no-store" -> whole (bare NoStore)
           "public" -> whole (bare Public)
           "private" -> whole (qualifiedDirective Private)
           "s-maxage" -> whole (numeric SMaxAge)
           "no-transform" -> whole (bare NoTransform)
           "must-revalidate" -> whole (bare MustRevalidate)
           "proxy-revalidate" -> whole (bare ProxyRevalidate)
           "immutable" -> whole immutableDirective
           "stale-while-revalidate" -> whole (numeric StaleWhileRevalidate)
           "stale-if-error" -> whole (numeric StaleIfError)
           "max-stale" -> whole maxStaleDirective
           "min-fresh" -> whole (numeric MinFresh)
           "only-if-cached" -> whole (bare OnlyIfCached)
           "must-understand" -> whole (bare MustUnderstand)
         |]
   )
  where
    numeric make = $(char '=') *> (make <$> directiveDelay)
    bare value = value <$ notFollowedBy $(char '=')
    maxStaleDirective = MaxStale <$> withOption $(char '=') (const (Just <$> directiveDelay)) (pure Nothing)
    immutableDirective = withOption $(char '=') (const (Immutable <$ (tokenBytes <|> (() <$ httpQuotedBytes)))) (pure Immutable)
{-# INLINE withKnownCacheDirective #-}


cacheControlHeaderParser :: ParserT st e (NonEmpty CacheControlDirective)
cacheControlHeaderParser = httpList directiveParser >>= maybe failed pure . NE.nonEmpty


-- | Parse a field line, including an explicitly empty directive list.
parseCacheControlDirectives :: ByteString -> Either String [CacheControlDirective]
parseCacheControlDirectives wire =
  withSingleCacheDirective wire (\directive -> Right [directive])
    (parseFieldLine "Cache-Control" (httpList directiveParser) wire)


-- | Parse a field line containing at least one directive.
parseCacheControlHeader :: ByteString -> Either String (NonEmpty CacheControlDirective)
parseCacheControlHeader wire =
  withSingleCacheDirective wire (\directive -> Right (directive :| []))
    (parseFieldLine "Cache-Control" cacheControlHeaderParser wire)


-- Recognise common whole field values. Everything else uses the same
-- list grammar, including mixed case, whitespace and larger delays.
withSingleCacheDirective :: ByteString -> (CacheControlDirective -> a) -> a -> a
withSingleCacheDirective wire success fallback
  | wire == "no-cache" = success (NoCache Nothing)
  | wire == "no-store" = success NoStore
  | wire == "only-if-cached" = success OnlyIfCached
  | Just digits <- BS.stripPrefix "max-age=" wire =
      withShortDecimal digits (success . MaxAge . fromIntegral) fallback
  | otherwise = fallback
{-# INLINE withSingleCacheDirective #-}


directivesBuilder :: NonEmpty CacheControlDirective -> BB.Builder
directivesBuilder = BB.byteString . M.toStrictByteString . renderCacheDirectives


directiveBuilder :: CacheControlDirective -> BB.Builder
directiveBuilder = BB.byteString . M.toStrictByteString . renderCacheDirective


renderCacheDirectives :: NonEmpty CacheControlDirective -> M.Builder
renderCacheDirectives = M.intersperse "," . fmap renderCacheDirective


data CacheDirectiveWire
  = DirectiveFlag !SBS.ShortByteString
  | DirectiveSeconds !SBS.ShortByteString {-# UNPACK #-} !Word
  | DirectiveFields !SBS.ShortByteString !ShortText
  | DirectiveExtension !ShortText !(Maybe ShortText)


cacheDirectiveWire :: CacheControlDirective -> CacheDirectiveWire
cacheDirectiveWire = \case
  Public -> DirectiveFlag "public"
  Private v -> maybe (DirectiveFlag "private") (DirectiveFields "private") v
  NoCache v -> maybe (DirectiveFlag "no-cache") (DirectiveFields "no-cache") v
  NoStore -> DirectiveFlag "no-store"
  MaxAge v -> DirectiveSeconds "max-age" v
  MaxStale v -> maybe (DirectiveFlag "max-stale") (DirectiveSeconds "max-stale") v
  MinFresh v -> DirectiveSeconds "min-fresh" v
  SMaxAge v -> DirectiveSeconds "s-maxage" v
  MustRevalidate -> DirectiveFlag "must-revalidate"
  MustUnderstand -> DirectiveFlag "must-understand"
  ProxyRevalidate -> DirectiveFlag "proxy-revalidate"
  NoTransform -> DirectiveFlag "no-transform"
  OnlyIfCached -> DirectiveFlag "only-if-cached"
  Immutable -> DirectiveFlag "immutable"
  StaleWhileRevalidate v -> DirectiveSeconds "stale-while-revalidate" v
  StaleIfError v -> DirectiveSeconds "stale-if-error" v
  Unknown key val -> DirectiveExtension key val
{-# INLINE cacheDirectiveWire #-}


encodeCacheDirectivesChecked :: NonEmpty CacheControlDirective -> Maybe ByteString
encodeCacheDirectivesChecked directives@(first :| rest)
  | all validCacheDirective directives =
      Just $ BSI.unsafeCreateUptoN capacity $ \ptr -> writeDirective ptr 0 first >>= writeRest ptr rest
  | otherwise = Nothing
  where
    capacity = foldl' (\n item -> add (add n 1) (directiveSize item)) (directiveSize first) rest
    add = BSI.checkedAdd "Cache-Control field"
    textSize = SBS.length . TS.toShortByteString
    directiveSize item = case cacheDirectiveWire item of
      DirectiveFlag name -> SBS.length name
      DirectiveSeconds name _ -> add (SBS.length name) 21
      DirectiveFields name value -> add (add (SBS.length name) 3) (textSize value)
      DirectiveExtension name Nothing -> textSize name
      DirectiveExtension name (Just value) ->
        let size = textSize value
        in add (add (textSize name) 3) (add size size)

    writeRest _ [] !offset = pure offset
    writeRest ptr (item : more) !offset = do
      start <- dWord8 ptr offset 44
      writeDirective ptr start item >>= writeRest ptr more
    writeDirective ptr offset item = case cacheDirectiveWire item of
      DirectiveFlag name -> copy ptr offset name
      DirectiveSeconds name seconds -> do
        end <- copy ptr offset name
        start <- dWord8 ptr end 61
        next <- Prim.runB Prim.word64Dec (fromIntegral seconds) (ptr `plusPtr` start)
        pure (next `minusPtr` ptr)
      DirectiveFields name value -> do
        end <- copy ptr offset name
        start <- dWord8 ptr end 61 >>= \equals -> dWord8 ptr equals 34
        close <- copy ptr start (TS.toShortByteString value)
        dWord8 ptr close 34
      DirectiveExtension name value -> do
        end <- copy ptr offset (TS.toShortByteString name)
        case value of
          Nothing -> pure end
          Just text -> do
            start <- dWord8 ptr end 61
            let bytes = TS.toShortByteString text
            if isHttpTokenBytes bytes then copy ptr start bytes else foldHttpQuotedShortBytes (dWord8 ptr) start bytes
    copy ptr offset bytes = do
      let size = SBS.length bytes
      SBS.copyToPtr bytes 0 (ptr `plusPtr` offset) size
      pure $! offset + size


renderCacheDirective :: CacheControlDirective -> M.Builder
renderCacheDirective item = case cacheDirectiveWire item of
  DirectiveFlag name -> M.shortByteString name
  DirectiveSeconds name v -> M.shortByteString name <> "=" <> M.wordDec v
  DirectiveFields name v -> M.shortByteString name <> "=" <> renderHttpQuotedString (TS.toText v)
  DirectiveExtension k v -> shortText k <> foldMap (("=" <>) . extensionValue) v
  where
    extensionValue value
      | not (BS.null bytes) && BC.all (\c -> CharSet.member c tokenCharSet) bytes = shortText value
      | otherwise = renderHttpQuotedString (TS.toText value)
      where
        bytes = TS.toByteString value
